;;; overleaf-project-magit.el --- Overleaf sync section for magit-status -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Adds an Overleaf section to `magit-status' buffers showing the sync
;; state and a diff of local changes against the last synced snapshot.

;;; Code:

(require 'overleaf-project)
(require 'magit)

;;;; Buffer-local state

(defvar-local overleaf-project-magit--remote-commit nil
  "SHA of the remote snapshot commit after `overleaf-project-magit-refresh-remote'.")

;;;; Helpers

(defun overleaf-project-magit--state-label (repo base-ref)
  "Return (LABEL . FACE) describing the sync state of REPO against BASE-REF."
  (let ((pending (overleaf-project--pending-state repo)))
    (cond
     (pending
      (let ((action (plist-get pending :action))
            (branch (plist-get pending :sync-branch)))
        (cons (format "pending %s on %s" action branch) 'error)))
     ((equal (overleaf-project--tree-id repo base-ref)
             (overleaf-project--tree-id repo "HEAD"))
      (cons "in sync" 'magit-dimmed))
     (t
      (cons "local changes" 'warning)))))

;;;; Section insertion

(defun overleaf-project-magit-insert-status ()
  "Insert an Overleaf section into the current `magit-status' buffer."
  (when-let* ((repo (magit-toplevel))
              (_managed (overleaf-project--managed-repo-p repo))
              (base-ref (overleaf-project--base-ref repo))
              (base-rev (overleaf-project--rev-parse-noerror repo base-ref)))
    (let* ((name (overleaf-project--project-name repo))
           (state (overleaf-project-magit--state-label repo base-ref))
           (label (car state))
           (face (cdr state)))
      (magit-insert-section (overleaf)
        (magit-insert-heading
          (format "Overleaf: %s (%s%s)"
                  (propertize name 'font-lock-face 'magit-section-heading)
                  (propertize label 'font-lock-face face)
                  (if overleaf-project-magit--refreshing
                      (propertize ", refreshing..." 'font-lock-face 'magit-dimmed)
                    "")))
        ;; Local changes diff (base..HEAD), collapsed by default
        (unless (equal (overleaf-project--tree-id repo base-ref)
                       (overleaf-project--tree-id repo "HEAD"))
          (magit-insert-section (overleaf-local nil t)
            (magit-insert-heading "Local changes (base..HEAD):")
            (magit-insert-section-body
              (magit--insert-diff nil
                "diff" base-rev "HEAD" "--no-prefix"))))
        ;; Remote changes (shown after r refresh)
        (when overleaf-project-magit--remote-commit
          (magit-insert-section (overleaf-remote nil t)
            (magit-insert-heading "Remote changes (base..remote):")
            (magit-insert-section-body
              (magit--insert-diff nil
                "diff" base-rev overleaf-project-magit--remote-commit
                "--no-prefix"))))))))

;;;; Remote refresh

(defvar-local overleaf-project-magit--refreshing nil
  "Non-nil while an async remote refresh is in progress.")

(defun overleaf-project-magit--curl-args (url zipfile headers)
  "Build curl argument list to download URL into ZIPFILE with HEADERS."
  (append
   '("--fail" "--silent" "--show-error" "--location")
   (apply #'append
          (mapcar (lambda (h) (list "-H" h)) headers))
   (list "--output" zipfile url)))

(defun overleaf-project-magit-refresh-remote ()
  "Asynchronously download the remote Overleaf snapshot and show remote changes.
Starts a background curl process; on completion, extracts the zip,
creates a temporary commit, and refreshes the magit buffer."
  (interactive)
  (let* ((repo (or (magit-toplevel)
                   (user-error "Not inside a Git repository")))
         (_ (unless (overleaf-project--managed-repo-p repo)
              (user-error "Not an Overleaf project")))
         (base-ref (overleaf-project--base-ref repo))
         (base-rev (or (overleaf-project--rev-parse-noerror repo base-ref)
                       (user-error "Base ref %s does not exist" base-ref)))
         (project-id (overleaf-project--project-id repo))
         (magit-buf (current-buffer))
         (zipfile (make-temp-file "overleaf-project." nil ".zip"))
         (temp-dir (make-temp-file "overleaf-project." t))
         (headers
          (overleaf-project--format-curl-headers
           (overleaf-project--base-headers
            (overleaf--project-page-url project-id))))
         (url (format "%s/project/%s/download/zip"
                      (overleaf--url) project-id))
         (curl (overleaf-project--ensure-executable
                overleaf-curl-executable)))
    (when overleaf-project-magit--refreshing
      (user-error "Remote refresh already in progress"))
    (setq overleaf-project-magit--refreshing t)
    (overleaf--message "Downloading remote snapshot...")
    (make-process
     :name "overleaf-remote-refresh"
     :command (cons curl
                    (overleaf-project-magit--curl-args url zipfile headers))
     :noquery t
     :sentinel
     (lambda (proc event)
       (unwind-protect
           (cond
            ((not (eq (process-status proc) 'exit))
             ;; still running or signaled — ignore
             nil)
            ((not (zerop (process-exit-status proc)))
             (ignore-errors (delete-file zipfile))
             (ignore-errors (delete-directory temp-dir t))
             (when (buffer-live-p magit-buf)
               (with-current-buffer magit-buf
                 (setq overleaf-project-magit--remote-commit nil)
                 (magit-refresh)))
             (message "Overleaf remote refresh failed: curl exited %d"
                      (process-exit-status proc)))
            (t
             (condition-case err
                 (progn
                   (overleaf-project--run
                    overleaf-unzip-executable
                    (list "-q" "-o" zipfile "-d" temp-dir))
                   (let* ((root (overleaf-project--normalize-extracted-root
                                 temp-dir))
                          (commit (overleaf-project--commit-directory
                                   repo root base-rev
                                   "overleaf remote snapshot")))
                     (when (buffer-live-p magit-buf)
                       (with-current-buffer magit-buf
                         (setq overleaf-project-magit--remote-commit commit)
                         (overleaf--message "Remote snapshot ready.")
                         (magit-refresh)))))
               (error
                (when (buffer-live-p magit-buf)
                  (with-current-buffer magit-buf
                    (setq overleaf-project-magit--remote-commit nil)
                    (magit-refresh)))
                (message "Overleaf remote refresh failed: %s"
                         (error-message-string err))))
             (ignore-errors (delete-file zipfile))
             (ignore-errors (delete-directory temp-dir t))))
         (when (buffer-live-p magit-buf)
           (with-current-buffer magit-buf
             (setq overleaf-project-magit--refreshing nil))))))))

;;;; Setup

;;;###autoload
(defun overleaf-project-magit-setup ()
  "Enable the Overleaf section in `magit-status' buffers."
  (magit-add-section-hook
   'magit-status-sections-hook
   #'overleaf-project-magit-insert-status
   'magit-insert-stashes
   nil))

(provide 'overleaf-project-magit)

;;; overleaf-project-magit.el ends here
