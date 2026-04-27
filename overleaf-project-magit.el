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

;;;; Customization

(defcustom overleaf-project-magit-auto-refresh-remote t
  "Whether `magit-status' refreshes should also refresh the Overleaf remote.

When non-nil, refreshes of an Overleaf-managed `magit-status' buffer
may start an asynchronous download of the latest remote snapshot.
Automatic downloads are throttled by
`overleaf-project-magit-auto-refresh-remote-interval'.  The follow-up
refresh that displays the downloaded snapshot does not start another
remote download."
  :type 'boolean
  :group 'overleaf-project)

(defcustom overleaf-project-magit-auto-refresh-remote-interval 300
  "Minimum seconds between automatic Overleaf remote refreshes in Magit.

Manual `overleaf-project-magit-refresh-remote' calls are not throttled.
Set this to nil to allow every eligible `magit-status' refresh to start
a remote refresh."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "No automatic throttle" nil))
  :group 'overleaf-project)

;;;; Buffer-local state

(defvar-local overleaf-project-magit--remote-commit nil
  "SHA of the remote snapshot commit after `overleaf-project-magit-refresh-remote'.")

(defvar-local overleaf-project-magit--refreshing nil
  "Non-nil while an async remote refresh is in progress.")

(defvar-local overleaf-project-magit--last-remote-refresh-time nil
  "Last time an Overleaf remote refresh started in this Magit buffer.")

(defvar-local overleaf-project-magit--suppress-next-auto-refresh nil
  "Non-nil means skip the next automatic remote refresh once.")

;;;; Helpers

(defun overleaf-project-magit--state-label (repo in-sync)
  "Return (LABEL . FACE) describing the sync state of REPO.
IN-SYNC is non-nil when the base tree matches HEAD."
  (let ((pending (overleaf-project--pending-state repo)))
    (cond
     (pending
      (let ((action (plist-get pending :action))
            (branch (plist-get pending :sync-branch)))
        (cons (format "pending %s on %s" action branch) 'error)))
     (in-sync (cons "in sync" 'magit-dimmed))
     (t       (cons "local changes" 'warning)))))

;;;; Section insertion

(defun overleaf-project-magit-insert-status ()
  "Insert an Overleaf section into the current `magit-status' buffer."
  (when-let* ((repo (magit-toplevel))
              (managed (overleaf-project--managed-repo-p repo))
              (base-ref (overleaf-project--base-ref repo))
              (base-rev (overleaf-project--rev-parse-noerror repo base-ref)))
    (let* ((name (overleaf-project--project-name repo))
           (in-sync (equal (overleaf-project--tree-id repo base-ref)
                           (overleaf-project--tree-id repo "HEAD")))
           (state (overleaf-project-magit--state-label repo in-sync))
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
			    (unless in-sync
			      (magit-insert-section (overleaf-local nil t)
						    (magit-insert-heading "Local changes (base..HEAD):")
						    (magit-insert-section-body
						     (let ((default-directory repo))
						       (magit--insert-diff nil
									   "diff" base-rev "HEAD" "--no-prefix")))))
			    ;; Remote changes (shown after r refresh)
			    (when overleaf-project-magit--remote-commit
			      (magit-insert-section (overleaf-remote nil t)
						    (magit-insert-heading "Remote changes (base..remote):")
						    (magit-insert-section-body
						     (let ((default-directory repo))
						       (magit--insert-diff nil
									   "diff" base-rev overleaf-project-magit--remote-commit
									   "--no-prefix")))))))))

;;;; Remote refresh

(defun overleaf-project-magit--auto-refresh-due-p ()
  "Return non-nil when this Magit buffer may auto-refresh the remote."
  (or (null overleaf-project-magit-auto-refresh-remote-interval)
      (null overleaf-project-magit--last-remote-refresh-time)
      (>= (- (float-time) overleaf-project-magit--last-remote-refresh-time)
          overleaf-project-magit-auto-refresh-remote-interval)))

(defun overleaf-project-magit--maybe-auto-refresh-remote ()
  "Refresh the Overleaf remote after `magit-status' buffer refreshes."
  (cond
   (overleaf-project-magit--suppress-next-auto-refresh
    (setq overleaf-project-magit--suppress-next-auto-refresh nil))
   ((and overleaf-project-magit-auto-refresh-remote
         (derived-mode-p 'magit-status-mode)
         (not overleaf-project-magit--refreshing)
         (overleaf-project-magit--auto-refresh-due-p))
    (when-let ((repo (magit-toplevel)))
      (when (overleaf-project--managed-repo-p repo)
        (condition-case err
            (overleaf-project-magit-refresh-remote)
          (error
	   (overleaf-project--debug
	    "Skipping automatic Overleaf remote refresh: %s"
	    (error-message-string err)))))))))

(defun overleaf-project-magit--finish-remote-refresh
    (magit-buf remote-commit &optional message)
  "Update MAGIT-BUF after a remote refresh.
REMOTE-COMMIT is the downloaded snapshot commit, or nil on failure.
MESSAGE is displayed before refreshing the Magit buffer when non-nil."
  (when (buffer-live-p magit-buf)
    (with-current-buffer magit-buf
      (setq overleaf-project-magit--remote-commit remote-commit)
      (setq overleaf-project-magit--refreshing nil)
      (setq overleaf-project-magit--suppress-next-auto-refresh t)
      (when message
        (overleaf-project--message "%s" message))
      (magit-refresh))))

(defun overleaf-project-magit--enable-status-buffer-hooks ()
  "Enable Overleaf Magit hooks in the current `magit-status' buffer."
  (add-hook 'magit-refresh-buffer-hook
            #'overleaf-project-magit--maybe-auto-refresh-remote
            nil
            t))

(defun overleaf-project-magit-refresh-remote ()
  "Asynchronously download the remote Overleaf snapshot and show remote changes.
Downloads, extracts, creates a temporary commit, and refreshes the
Magit buffer without doing the heavy work in the foreground."
  (interactive)
  (let* ((repo (or (magit-toplevel)
                   (user-error "Not inside a Git repository")))
         (_ (unless (overleaf-project--managed-repo-p repo)
              (user-error "Not an Overleaf project")))
         (magit-buf (current-buffer)))
    (when overleaf-project-magit--refreshing
      (user-error "Remote refresh already in progress"))
    (overleaf-project--set-repo-url repo)
    (let* ((base-ref (overleaf-project--base-ref repo))
           (base-rev (or (overleaf-project--rev-parse-noerror repo base-ref)
                         (user-error "Base ref %s does not exist" base-ref)))
           (project-id (overleaf-project--project-id repo)))
      (overleaf-project--with-repo-log-context repo
					       (setq overleaf-project-magit--refreshing t)
					       (setq overleaf-project-magit--last-remote-refresh-time (float-time))
					       (overleaf-project--async-start
						"Overleaf Magit remote refresh"
						(lambda ()
						  (overleaf-project--set-repo-url repo)
						  (overleaf-project--with-downloaded-snapshot
						   project-id
						   (lambda (root)
						     (overleaf-project--commit-directory
						      repo
						      root
						      base-rev
						      "overleaf remote snapshot"))))
						:key (format "magit-remote:%s"
							     (directory-file-name (expand-file-name repo)))
						:on-success
						(lambda (commit)
						  (overleaf-project-magit--finish-remote-refresh
						   magit-buf
						   commit
						   "Remote snapshot ready."))
						:on-error
						(lambda (message)
						  (overleaf-project-magit--finish-remote-refresh magit-buf nil)
						  (overleaf-project--warn "Overleaf remote refresh failed: %s" message)))))))

;;;; Setup

;;;###autoload
(defun overleaf-project-magit-setup ()
  "Enable the Overleaf section in `magit-status' buffers."
  (magit-add-section-hook
   'magit-status-sections-hook
   #'overleaf-project-magit-insert-status
   'magit-insert-stashes
   nil)
  (add-hook 'magit-status-mode-hook
            #'overleaf-project-magit--enable-status-buffer-hooks))

(provide 'overleaf-project-magit)

;;; overleaf-project-magit.el ends here
