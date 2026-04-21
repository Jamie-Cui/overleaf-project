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
          (format "Overleaf: %s (%s)"
                  (propertize name 'font-lock-face 'magit-section-heading)
                  (propertize label 'font-lock-face face)))
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

;;;; Section keymap

(defvar-keymap magit-overleaf-section-map
  :doc "Keymap for the `overleaf' section in magit-status."
  "p" #'overleaf-project-push
  "l" #'overleaf-project-pull
  "r" #'overleaf-project-magit-refresh-remote
  "b" #'overleaf-project-browse-remote)

;;;; Remote refresh

(defun overleaf-project-magit-refresh-remote ()
  "Download the remote Overleaf snapshot and show remote changes.
Creates a temporary commit from the remote zip and stores its SHA
so the section can display a remote-changes diff."
  (interactive)
  (let* ((repo (or (magit-toplevel)
                   (user-error "Not inside a Git repository")))
         (_ (unless (overleaf-project--managed-repo-p repo)
              (user-error "Not an Overleaf project")))
         (base-ref (overleaf-project--base-ref repo))
         (base-rev (or (overleaf-project--rev-parse-noerror repo base-ref)
                       (user-error "Base ref %s does not exist" base-ref)))
         (project-id (overleaf-project--project-id repo)))
    (condition-case err
        (let ((snapshot nil))
          (unwind-protect
              (progn
                (setq snapshot
                      (overleaf-project--download-snapshot project-id))
                (overleaf--message "Creating temporary commit...")
                (let ((commit
                       (overleaf-project--commit-directory
                        repo
                        (overleaf-project--snapshot-root snapshot)
                        base-rev
                        "overleaf remote snapshot")))
                  (setq overleaf-project-magit--remote-commit commit)
                  (overleaf--message "Remote snapshot ready.")
                  (magit-refresh)))
            (when snapshot
              (ignore-errors
                (delete-directory
                 (overleaf-project--snapshot-temp-dir snapshot) t)))))
      (error
       (setq overleaf-project-magit--remote-commit nil)
       (magit-refresh)
       (message "Overleaf remote refresh failed: %s"
                (error-message-string err))))))

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
