;;; overleaf-project-commands.el --- Interactive commands for overleaf-project -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is not part of GNU Emacs.

;;; Commentary:

;; User-facing Overleaf project commands and command map.

;;; Code:

(require 'overleaf-project-auth)
(require 'overleaf-project-core)
(require 'overleaf-project-http)
(require 'overleaf-project-sync)

;;;; Command helpers

(defun overleaf-project--repo-async-key (repo)
  "Return the async lock key for REPO."
  (format "repo:%s" (directory-file-name (expand-file-name repo))))

(defun overleaf-project--ensure-authenticated-async (action continuation)
  "Ensure cookies are usable, then call CONTINUATION in the foreground.
When authentication is needed, run browser authentication in the
background before calling CONTINUATION."
  (let ((state (overleaf-project--cookie-state)))
    (if (eq (plist-get state :status) 'valid)
	(funcall continuation)
      (let ((reason (overleaf-project--authentication-needed-reason state)))
	(if (or noninteractive
		(not
		 (let ((use-dialog-box nil))
                   (y-or-n-p
                    (format "%s Re-run `overleaf-project-authenticate` now? "
                            reason)))))
            (user-error
             "%s Run `overleaf-project-authenticate` before %s"
             reason
             (or action "continuing"))
          (overleaf-project--start-authentication-async
           overleaf-project-url
           (lambda (_full-cookies)
             (funcall continuation))))))))

(defun overleaf-project--read-project-async (url continuation)
  "Fetch projects from URL in the background, then call CONTINUATION."
  (overleaf-project--async-start
   (format "Overleaf project list from %s" (overleaf-project--url-host))
   (lambda ()
     (overleaf-project-list url))
   :key (format "project-list:%s" url)
   :on-success
   (lambda (projects)
     (funcall continuation (overleaf-project--select-project projects)))))

(defun overleaf-project--clone-target-directory (project target-directory)
  "Return the target directory for cloning PROJECT."
  (directory-file-name
   (expand-file-name
    (or target-directory
        (read-file-name
         "Clone to directory: "
         default-directory
         (expand-file-name
          (overleaf-project--sanitize-name (plist-get project :name))
          default-directory)
         nil
         (overleaf-project--sanitize-name (plist-get project :name)))))))

(defun overleaf-project--validate-clone-target (target)
  "Signal if TARGET is not a valid clone target."
  (when (and (file-exists-p target)
             (not (file-directory-p target)))
    (user-error "Target path %s exists and is not a directory" target))
  (unless (overleaf-project--directory-empty-p target)
    (user-error "Target directory %s is not empty" target)))

(defun overleaf-project--clone-selected-project (url project target)
  "Synchronously clone PROJECT from URL into TARGET."
  (let ((overleaf-project-url url)
        (target (directory-file-name (expand-file-name target)))
        (repo nil))
    (overleaf-project-log-with-context
     (overleaf-project--log-context-for-project project target)
     (overleaf-project--validate-clone-target target)
     (overleaf-project--with-downloaded-snapshot
      (plist-get project :id)
      (lambda (snapshot-root)
        (make-directory target t)
        (overleaf-project--copy-directory-contents snapshot-root target)
        (setq repo target)
        (overleaf-project--git-output repo "init")
        (overleaf-project--write-repo-metadata repo project)
        (overleaf-project--prepare-sync-metadata-repo repo)
        (overleaf-project--git-output repo "add" "--all" ".")
        (apply
         #'overleaf-project--git-output
         repo
         (append
          (overleaf-project--git-identity-args repo)
          '("commit" "-m" "chore: import project from Overleaf")))
        (overleaf-project--set-base-ref repo "HEAD")
        (overleaf-project--message
         "Cloned `%s' into %s"
         (plist-get project :name)
         target))))))

(defun overleaf-project--clone-1 (&optional url target-directory)
  "Synchronously clone a full Overleaf project into TARGET-DIRECTORY."
  (let* ((url (or url (overleaf-project--url)))
         (project nil)
         (target nil))
    (setq overleaf-project-url url)
    (overleaf-project--ensure-authenticated "cloning from Overleaf")
    (setq project (overleaf-project--read-project url))
    (setq target
          (overleaf-project--clone-target-directory project target-directory))
    (overleaf-project--validate-clone-target target)
    (overleaf-project--clone-selected-project url project target)))

(defun overleaf-project--clone-async (&optional url target-directory)
  "Start an asynchronous clone into TARGET-DIRECTORY."
  (let ((url (or url (overleaf-project--url))))
    (setq overleaf-project-url url)
    (overleaf-project--ensure-authenticated-async
     "cloning from Overleaf"
     (lambda ()
       (overleaf-project--read-project-async
        url
        (lambda (project)
          (let ((target
                 (overleaf-project--clone-target-directory
                  project
                  target-directory)))
            (overleaf-project--validate-clone-target target)
            (overleaf-project-log-with-context
             (overleaf-project--log-context-for-project project target)
             (overleaf-project--async-start
              (format "Overleaf clone `%s'" (plist-get project :name))
              (lambda ()
                (overleaf-project--clone-selected-project url project target))
              :key (format "clone:%s" target))))))))))

(defun overleaf-project--init-confirm-p (repo current-id current-name project)
  "Return non-nil if initializing REPO for PROJECT should continue."
  (or (not current-id)
      (yes-or-no-p
       (if (string= current-id (plist-get project :id))
           (format
            "Reinitialize the Overleaf base snapshot for `%s` against `%s'? "
            repo
            (or current-name current-id))
         (format
          "Rebind `%s' from Overleaf project `%s' to `%s'? "
          repo
          (or current-name current-id)
          (plist-get project :name))))))

(defun overleaf-project--init-selected-project (repo project)
  "Synchronously bind REPO to PROJECT and initialize its base snapshot."
  (overleaf-project-log-with-context
   (overleaf-project--log-context-for-project project repo)
   (overleaf-project--ensure-no-pending-action repo "reconfiguring the repository")
   (overleaf-project--prepare-sync-metadata-repo repo)
   (overleaf-project--with-downloaded-snapshot
    (plist-get project :id)
    (lambda (snapshot-root)
      (overleaf-project--initialize-base-ref repo project snapshot-root)
      (overleaf-project--message
       "Configured `%s' to track Overleaf project `%s' without pulling or pushing"
       repo
       (plist-get project :name))))))

(defun overleaf-project--init-1 (&optional directory url confirm)
  "Synchronously bind DIRECTORY to an Overleaf project on URL.
When CONFIRM is non-nil, ask before rebinding an existing project."
  (let* ((repo (overleaf-project--require-repo directory))
         (current-id nil)
         (current-name nil)
         (project nil))
    (overleaf-project--ensure-no-pending-action repo "reconfiguring the repository")
    (overleaf-project--set-repo-url repo url)
    (overleaf-project--prepare-sync-metadata-repo repo)
    (overleaf-project--ensure-authenticated "configuring the Overleaf project")
    (setq current-id (overleaf-project--git-config-get repo "overleaf-project.projectId"))
    (setq current-name (overleaf-project--git-config-get repo "overleaf-project.projectName"))
    (setq project (overleaf-project--read-project overleaf-project-url))
    (when (and confirm
               (not
                (overleaf-project--init-confirm-p
                 repo current-id current-name project)))
      (user-error "Aborted"))
    (overleaf-project--init-selected-project repo project)))

(defun overleaf-project--init-async (&optional directory url)
  "Start an asynchronous Overleaf project initialization."
  (let* ((repo (overleaf-project--require-repo directory))
         (current-id nil)
         (current-name nil))
    (overleaf-project--ensure-no-pending-action repo "reconfiguring the repository")
    (overleaf-project--set-repo-url repo url)
    (setq current-id (overleaf-project--git-config-get repo "overleaf-project.projectId"))
    (setq current-name (overleaf-project--git-config-get repo "overleaf-project.projectName"))
    (overleaf-project--ensure-authenticated-async
     "configuring the Overleaf project"
     (lambda ()
       (overleaf-project--read-project-async
        overleaf-project-url
        (lambda (project)
          (when (not
                 (overleaf-project--init-confirm-p
                  repo current-id current-name project))
            (user-error "Aborted"))
          (overleaf-project-log-with-context
           (overleaf-project--log-context-for-project project repo)
           (overleaf-project--async-start
            (format "Overleaf init `%s'" repo)
            (lambda ()
              (overleaf-project--init-selected-project repo project))
            :key (overleaf-project--repo-async-key repo)))))))))

(defun overleaf-project--push-unstaged-action (repo noerror)
  "Return the unstaged-change action for pushing REPO.
When NOERROR is non-nil, do not prompt."
  (let ((status (overleaf-project--read-repo-status repo)))
    (when (overleaf-project--repo-status-unmerged status)
      (user-error
       "Repository %s has unresolved merge conflicts; resolve them before pushing"
       repo))
    (if (overleaf-project--repo-status-unstaged status)
        (progn
          (when noerror
            (user-error
             "Overleaf push requires a clean working tree; stage all changes or stash them first"))
          (unless
              (y-or-n-p
               (format
                "Repository %s has unstaged changes. Stage all changes and continue with Overleaf push? "
                repo))
            (user-error
             "Overleaf push requires a clean working tree; stage all changes or stash them first"))
          'stage)
      'error)))

(defun overleaf-project--push-async (repo noerror)
  "Start an asynchronous push for REPO.
When NOERROR is non-nil, demote setup and background errors to warnings."
  (overleaf-project--with-repo-log-context repo
					   (condition-case err
					       (let* ((pending nil)
						      (unstaged-action nil)
						      (name nil))
						 (overleaf-project--set-repo-url repo)
						 (setq pending (overleaf-project--pending-state repo))
						 (if pending
						     (overleaf-project--ensure-clean-working-tree
						      repo
						      "finishing the pending Overleaf operation")
						   (setq unstaged-action
							 (overleaf-project--push-unstaged-action repo noerror)))
						 (setq name (format "Overleaf push `%s'"
								    (overleaf-project--project-name repo)))
						 (cl-labels
						     ((start ()
							(overleaf-project--async-start
							 name
							 (lambda ()
							   (overleaf-project--push-1 repo unstaged-action t))
							 :key (overleaf-project--repo-async-key repo)
							 :on-error
							 (lambda (message)
							   (if noerror
							       (overleaf-project--warn
								"Automatic Overleaf push failed for %s: %s"
								repo message)
							     (overleaf-project--warn "%s failed: %s" name message))))))
						   (if noerror
						       (progn
							 (overleaf-project--get-cookies)
							 (start))
						     (overleaf-project--ensure-authenticated-async
						      "pushing to Overleaf"
						      #'start))))
					     (error
					      (if noerror
						  (overleaf-project--warn "Automatic Overleaf push failed for %s: %s"
									  repo (error-message-string err))
						(signal (car err) (cdr err)))))))

(defun overleaf-project--overwrite-remote-async (repo)
  "Start an asynchronous remote overwrite for REPO."
  (overleaf-project--with-repo-log-context repo
					   (overleaf-project--ensure-no-pending-action repo "overwriting the Overleaf remote")
					   (overleaf-project--set-repo-url repo)
					   (when (not
						  (yes-or-no-p
						   (format
						    "Overwrite Overleaf project `%s' with local HEAD? This will replace remote files. "
						    (overleaf-project--project-name repo))))
					     (user-error "Aborted"))
					   (let ((unstaged-action
						  (overleaf-project--push-unstaged-action repo nil)))
					     (overleaf-project--ensure-authenticated-async
					      "overwriting the Overleaf remote"
					      (lambda ()
						(overleaf-project--async-start
						 (format "Overleaf remote overwrite `%s'"
							 (overleaf-project--project-name repo))
						 (lambda ()
						   (overleaf-project--overwrite-remote-1 repo unstaged-action t))
						 :key (overleaf-project--repo-async-key repo)))))))

(defun overleaf-project--pull-async (repo)
  "Start an asynchronous pull for REPO."
  (overleaf-project--with-repo-log-context repo
					   (overleaf-project--set-repo-url repo)
					   (let ((pending (overleaf-project--pending-state repo)))
					     (when pending
					       (pcase (plist-get pending :action)
						 ('pull
						  (user-error
						   "Unresolved merge conflicts from a previous pull; resolve them, commit, then run `overleaf-project-push'"))
						 ('push
						  (user-error
						   "Pending Overleaf push exists on branch `%s'; finish it before pulling"
						   (plist-get pending :sync-branch)))))
					     (overleaf-project--ensure-clean-working-tree repo "pulling from Overleaf"))
					   (overleaf-project--ensure-authenticated-async
					    "pulling from Overleaf"
					    (lambda ()
					      (overleaf-project--async-start
					       (format "Overleaf pull `%s'" (overleaf-project--project-name repo))
					       (lambda ()
						 (overleaf-project--pull-1 repo t))
					       :key (overleaf-project--repo-async-key repo))))))

;;;; Interactive commands

;;;###autoload
(defun overleaf-project-clone (&optional url target-directory)
  "Clone a full Overleaf project into TARGET-DIRECTORY.
If URL is nil, use `overleaf-project-url'."
  (interactive)
  (if (and (called-interactively-p 'interactive)
           (overleaf-project--async-command-enabled-p))
      (overleaf-project--clone-async url target-directory)
    (overleaf-project--clone-1 url target-directory)))

;;;###autoload
(defun overleaf-project-init (&optional directory url)
  "Bind the Git repo in DIRECTORY to a remote Overleaf project on URL.
The command stores project metadata and initializes the hidden base
snapshot used by later `overleaf-project-push' and
`overleaf-project-pull' runs, but does not automatically pull or push."
  (interactive)
  (let ((interactive-p (called-interactively-p 'interactive)))
    (if (and interactive-p
             (overleaf-project--async-command-enabled-p))
        (overleaf-project--init-async directory url)
      (overleaf-project--init-1 directory url interactive-p))))

;;;###autoload
(defun overleaf-project-push (&optional directory noerror)
  "Push the current Git repo to its configured Overleaf project.
Staged changes are committed automatically before the remote snapshot is
fetched.  When unstaged changes exist, prompt whether to stage them
first.

If a pending pull exists (merge conflict from a previous pull), verifies
the merge is complete and uploads the merged result.  If the remote has
diverged and no pending pull exists, signals an error asking you to run
`overleaf-project-pull' first.

When NOERROR is non-nil, silently return nil if DIRECTORY is not a
managed Overleaf repo, and demote push errors to warnings.  This is
useful for hooks such as `git-commit-post-finish-hook'."
  (interactive)
  (let* ((repo (or (and directory (overleaf-project-root directory))
                   (overleaf-project-root default-directory))))
    (cond
     ((not (and repo (overleaf-project--managed-repo-p repo)))
      (if noerror
          nil
        (user-error "Repository %s is not configured as an Overleaf project"
                    (or repo default-directory))))
     ((or (and (called-interactively-p 'interactive)
               (overleaf-project--async-command-enabled-p))
          (and noerror
               (overleaf-project--async-command-enabled-p)))
      (overleaf-project--push-async repo noerror))
     (noerror
      (overleaf-project--with-repo-log-context repo
					       (condition-case err
						   (overleaf-project--push-1 repo)
						 (error
						  (overleaf-project--warn "Automatic Overleaf push failed for %s: %s"
									  repo (error-message-string err))))))
     (t
      (overleaf-project--push-1 repo)))))

(defun overleaf-project--push-1 (repo &optional unstaged-action skip-auth)
  "Internal: perform the actual push for managed REPO.
UNSTAGED-ACTION is passed to
`overleaf-project--prepare-working-tree-for-sync'.  When SKIP-AUTH is
non-nil, assume the caller already checked authentication."
  (overleaf-project--with-repo-log-context repo
					   (let ((pending nil)
						 (project-id nil))
					     (overleaf-project--set-repo-url repo)
					     (overleaf-project--prepare-sync-metadata-repo repo)
					     (setq pending (overleaf-project--pending-state repo))
					     (unless skip-auth
					       (overleaf-project--ensure-authenticated "pushing to Overleaf"))
					     (if pending
						 (overleaf-project--ensure-clean-working-tree repo "finishing the pending Overleaf operation")
					       (overleaf-project--prepare-working-tree-for-sync repo unstaged-action))
					     (setq project-id (overleaf-project--project-id repo))
					     (overleaf-project--with-remote-state
					      project-id
					      (lambda (remote-root remote-table)
						(pcase (and pending (plist-get pending :action))
						  ('pull
						   (overleaf-project--finalize-pending-pull
						    repo pending remote-root remote-table))
						  ('push
						   (overleaf-project--finalize-pending-push
						    repo pending remote-root remote-table))
						  (_
						   (when pending
						     (user-error "Unknown pending Overleaf action `%s'"
								 (plist-get pending :action)))
						   (overleaf-project--fresh-push repo remote-root remote-table))))))))

;;;###autoload
(defun overleaf-project-overwrite-remote (&optional directory)
  "Overwrite the configured Overleaf project with the current Git repo.
Like `overleaf-project-push', staged changes are committed automatically
before upload.  Unlike `overleaf-project-push', remote Overleaf changes
are replaced by the local `HEAD' snapshot."
  (interactive)
  (let* ((repo (overleaf-project--require-managed-repo directory)))
    (if (and (called-interactively-p 'interactive)
             (overleaf-project--async-command-enabled-p))
        (overleaf-project--overwrite-remote-async repo)
      (overleaf-project--overwrite-remote-1 repo nil nil))))

;;;###autoload
(define-obsolete-function-alias
  'overleaf-project-push-force
  'overleaf-project-overwrite-remote
  "2.0.0")

(defun overleaf-project--overwrite-remote-1
    (repo &optional unstaged-action skip-auth)
  "Synchronously overwrite Overleaf with REPO.
UNSTAGED-ACTION is passed to
`overleaf-project--prepare-working-tree-for-sync'.  When SKIP-AUTH is
non-nil, assume the caller already checked authentication."
  (overleaf-project--with-repo-log-context repo
					   (let ((project-id nil)
						 (context nil))
					     (overleaf-project--ensure-no-pending-action repo "overwriting the Overleaf remote")
					     (overleaf-project--set-repo-url repo)
					     (overleaf-project--prepare-sync-metadata-repo repo)
					     (setq project-id (overleaf-project--project-id repo))
					     (unless skip-auth
					       (overleaf-project--ensure-authenticated "overwriting the Overleaf remote"))
					     (overleaf-project--prepare-working-tree-for-sync repo unstaged-action)
					     (overleaf-project--with-remote-state
					      project-id
					      (lambda (remote-root remote-table)
						(setq context (overleaf-project--read-sync-state repo remote-root))
						(if (memq (plist-get context :status) '(in-sync head-matches-remote))
						    (overleaf-project--note-matching-sync-state
						     repo
						     (plist-get context :head)
						     project-id
						     remote-table)
						  (overleaf-project--upload-head-and-set-base
						   repo
						   (plist-get context :head)
						   project-id
						   remote-root
						   remote-table
						   "Overwrote Overleaf project `%s' with local HEAD"
						   (overleaf-project--project-name repo))))))))

;;;###autoload
(defun overleaf-project-pull (&optional directory)
  "Pull the latest Overleaf snapshot into the current Git repo.
The working tree must be clean before pulling.

When the local branch has diverged from the remote, performs a
`git merge --no-ff --no-edit' directly on the current branch.  If the
merge succeeds, records the downloaded remote snapshot as the base so a
later `overleaf-project-push' uploads the merged result.  If there are
conflicts, records a pending-pull state and prompts you to resolve them,
commit, then run `overleaf-project-push' to complete the sync."
  (interactive)
  (let* ((repo (overleaf-project--require-managed-repo directory)))
    (if (and (called-interactively-p 'interactive)
             (overleaf-project--async-command-enabled-p))
        (overleaf-project--pull-async repo)
      (overleaf-project--pull-1 repo nil))))

(defun overleaf-project--pull-1 (repo &optional skip-auth)
  "Synchronously pull the latest Overleaf snapshot into REPO.
When SKIP-AUTH is non-nil, assume the caller already checked
authentication."
  (overleaf-project--with-repo-log-context repo
					   (let ((pending (overleaf-project--pending-state repo)))
					     (overleaf-project--set-repo-url repo)
					     (overleaf-project--prepare-sync-metadata-repo repo)
					     (when pending
					       (pcase (plist-get pending :action)
						 ('pull
						  (user-error
						   "Unresolved merge conflicts from a previous pull; resolve them, commit, then run `overleaf-project-push'"))
						 ('push
						  (user-error
						   "Pending Overleaf push exists on branch `%s'; finish it before pulling"
						   (plist-get pending :sync-branch)))))
					     (overleaf-project--ensure-clean-working-tree repo "pulling from Overleaf")
					     (unless skip-auth
					       (overleaf-project--ensure-authenticated "pulling from Overleaf"))
					     (overleaf-project--with-downloaded-snapshot
					      (overleaf-project--project-id repo)
					      (lambda (remote-root)
						(overleaf-project--fresh-pull repo remote-root))))))



;;;###autoload
(defun overleaf-project-browse-remote (&optional directory)
  "Open the configured Overleaf project in a browser."
  (interactive)
  (let* ((repo (or (and directory (overleaf-project-root directory))
                   (overleaf-project-root default-directory))))
    (if repo
        (progn
          (overleaf-project--set-repo-url repo)
          (browse-url
           (overleaf-project--project-page-url
            (overleaf-project--project-id repo))))
      (if (and (called-interactively-p 'interactive)
               (overleaf-project--async-command-enabled-p))
          (overleaf-project--ensure-authenticated-async
           "selecting an Overleaf project"
           (lambda ()
             (overleaf-project--read-project-async
              overleaf-project-url
              (lambda (project)
                (browse-url
                 (overleaf-project--project-page-url
                  (plist-get project :id)))))))
        (browse-url
         (overleaf-project--project-page-url
          (plist-get (overleaf-project--read-project) :id)))))))

;;;; Command map

;;;###autoload
(defvar-keymap overleaf-project-command-map
  "a" #'overleaf-project-authenticate
  "b" #'overleaf-project-browse-remote
  "c" #'overleaf-project-clone
  "l" #'overleaf-project-pull
  "p" #'overleaf-project-push
  "s" #'overleaf-project-push)


(provide 'overleaf-project-commands)

;;; overleaf-project-commands.el ends here
