;;; overleaf-project-sync.el --- Snapshot sync internals for overleaf-project -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Remote sync metadata, local snapshots, and Git-backed sync logic.

;;; Code:

(require 'json)
(require 'overleaf-project-core)
(require 'overleaf-project-http)

;;;; Remote sync metadata helpers

(defun overleaf-project--sync-metadata-relative-path ()
  "Return the configured root-level sync metadata path."
  (when overleaf-project-sync-metadata-enabled
    (let ((path (string-trim overleaf-project-sync-metadata-file)))
      (cond
       ((string-empty-p path)
        (user-error "`overleaf-project-sync-metadata-file' cannot be empty"))
       ((file-name-absolute-p path)
        (user-error "`overleaf-project-sync-metadata-file' must be relative"))
       ((string-match-p "/" path)
        (user-error "`overleaf-project-sync-metadata-file' must be root-level"))
       ((string-match-p "\\`\\.\\.\\'" path)
        (user-error "`overleaf-project-sync-metadata-file' cannot be `..'"))
       (t path)))))

(defun overleaf-project--sync-metadata-path-p (path)
  "Return non-nil if PATH is the reserved sync metadata path."
  (and overleaf-project-sync-metadata-enabled
       (string= path (overleaf-project--sync-metadata-relative-path))))

(defun overleaf-project--sync-metadata-file-in-root (root)
  "Return the sync metadata file path inside ROOT."
  (expand-file-name (overleaf-project--sync-metadata-relative-path) root))

(defun overleaf-project--read-sync-metadata-file (file)
  "Read sync metadata from FILE, returning nil if FILE is invalid."
  (when (file-regular-p file)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents file)
          (json-parse-buffer :object-type 'plist :array-type 'list))
      (error
       (overleaf-project--warn
        "Ignoring invalid Overleaf sync metadata file %s: %s"
        file
        (error-message-string err))
       nil))))

(defun overleaf-project--extract-remote-sync-metadata (root)
  "Read and remove the remote sync metadata file from ROOT.
The file is removed so downloaded snapshots compare only user project
content."
  (when overleaf-project-sync-metadata-enabled
    (let ((file (overleaf-project--sync-metadata-file-in-root root)))
      (cond
       ((file-regular-p file)
        (prog1 (overleaf-project--read-sync-metadata-file file)
          (delete-file file)))
       ((file-exists-p file)
        (overleaf-project--warn
         "Ignoring reserved Overleaf sync metadata path because it is not a file: %s"
         file)
        (if (file-directory-p file)
            (delete-directory file t)
          (delete-file file))
        nil)))))

(defun overleaf-project--git-object-id-p (value)
  "Return non-nil if VALUE looks like a full Git object id."
  (and (stringp value)
       (string-match-p "\\`[[:xdigit:]]\\{40,64\\}\\'" value)))

(defun overleaf-project--remote-sync-metadata-commit (repo remote-tree)
  "Return the Git commit recorded by remote metadata if it matches REMOTE-TREE."
  (let* ((metadata overleaf-project--remote-sync-metadata)
         (commit (plist-get metadata :localCommit))
         (tree (plist-get metadata :localTree)))
    (when (and overleaf-project-sync-metadata-enabled
               (overleaf-project--git-object-id-p commit)
               (overleaf-project--git-object-id-p tree)
               (string= tree remote-tree))
      (when-let* ((resolved
                   (overleaf-project--git-output-noerror
                    repo
                    "rev-parse"
                    "--verify"
                    (format "%s^{commit}" commit))))
        (when (string= (overleaf-project--tree-id repo resolved) remote-tree)
          (overleaf-project--debug
           "Remote sync metadata maps snapshot to local commit %s"
           resolved)
          resolved)))))

(defun overleaf-project--sync-metadata-json (repo revision project-id)
  "Return JSON sync metadata for REVISION in REPO and PROJECT-ID."
  (let* ((commit (overleaf-project--rev-parse repo revision))
         (tree (overleaf-project--tree-id repo commit)))
    (concat
     (json-encode
      `(:schema 1
		:tool "overleaf-project"
		:projectId ,project-id
		:overleafUrl ,(overleaf-project--url)
		:localCommit ,commit
		:localTree ,tree
		:syncedAt ,(format-time-string
			    "%Y-%m-%dT%H:%M:%SZ"
			    (current-time)
			    t)))
     "\n")))

(defun overleaf-project--ensure-sync-metadata-ignored (repo)
  "Add the reserved sync metadata file to REPO's local Git exclude file."
  (when overleaf-project-sync-metadata-enabled
    (let* ((path (overleaf-project--sync-metadata-relative-path))
           (git-dir (expand-file-name ".git" repo))
           (exclude-file (expand-file-name "info/exclude" git-dir)))
      (when (file-directory-p git-dir)
        (make-directory (file-name-directory exclude-file) t)
        (with-temp-buffer
          (when (file-readable-p exclude-file)
            (insert-file-contents exclude-file))
          (goto-char (point-min))
          (unless (re-search-forward
                   (format "^%s$" (regexp-quote path))
                   nil
                   t)
            (goto-char (point-max))
            (unless (or (bobp) (= (char-before) ?\n))
              (insert "\n"))
            (insert path "\n")
            (write-region (point-min) (point-max) exclude-file nil 'silent)))))))

(defun overleaf-project--ensure-sync-metadata-untracked (repo)
  "Signal if REPO tracks the reserved sync metadata file."
  (when overleaf-project-sync-metadata-enabled
    (let ((path (overleaf-project--sync-metadata-relative-path)))
      (when (overleaf-project--git-output-noerror
             repo
             "ls-files"
             "--error-unmatch"
             "--"
             path)
        (user-error
         "`%s' is reserved for Overleaf sync metadata; remove it from Git tracking"
         path)))))

(defun overleaf-project--prepare-sync-metadata-repo (repo)
  "Prepare REPO for remote sync metadata bookkeeping."
  (overleaf-project--ensure-sync-metadata-ignored repo)
  (overleaf-project--ensure-sync-metadata-untracked repo))

(defun overleaf-project--with-downloaded-snapshot (project-id function)
  "Download PROJECT-ID, call FUNCTION with the snapshot root, then clean up."
  (overleaf-project-log-with-context
   (overleaf-project-log-make-context
    :project-id project-id
    :url (overleaf-project--url))
   (let ((snapshot nil))
     (unwind-protect
         (progn
           (setq snapshot (overleaf-project--download-snapshot project-id))
           (let ((overleaf-project--remote-sync-metadata
                  (overleaf-project--extract-remote-sync-metadata
                   (overleaf-project--snapshot-root snapshot))))
             (funcall function (overleaf-project--snapshot-root snapshot))))
       (when snapshot
         (ignore-errors
           (delete-directory
            (overleaf-project--snapshot-temp-dir snapshot)
            t)))))))
(defun overleaf-project--with-remote-state (project-id function)
  "Download PROJECT-ID and call FUNCTION with the remote root and entity table."
  (overleaf-project--with-downloaded-snapshot
   project-id
   (lambda (remote-root)
     (funcall function
              remote-root
              (overleaf-project--fetch-remote-table project-id)))))
;;;; Local snapshot helpers

(defun overleaf-project--scan-local-tree (root)
  "Return local directory and file tables rooted at ROOT."
  (let ((dirs (make-hash-table :test #'equal))
        (files (make-hash-table :test #'equal)))
    (puthash "" root dirs)
    (cl-labels
        ((walk (dir)
           (dolist (entry (directory-files dir t nil t))
             (unless (member (file-name-nondirectory entry) '("." ".." ".git"))
               (let ((relative (file-relative-name entry root)))
                 (unless (overleaf-project--sync-metadata-path-p relative)
                   (if (file-directory-p entry)
                       (progn
                         (puthash relative entry dirs)
                         (walk entry))
                     (puthash relative entry files))))))))
      (walk root))
    `(:dirs ,dirs :files ,files)))

(defun overleaf-project--make-temp-index-path ()
  "Return a fresh path for a temporary Git index file.
The path itself does not exist yet, because Git expects to create the
index file on first use."
  (let ((path (make-temp-file "overleaf-index.")))
    (delete-file path)
    path))

(defun overleaf-project--materialize-commit (repo revision)
  "Write REVISION from REPO to a temporary directory and return it."
  (let* ((temp-dir (make-temp-file "overleaf-materialized." t))
         (index-file (overleaf-project--make-temp-index-path))
         (env (list (concat "GIT_INDEX_FILE=" index-file))))
    (unwind-protect
        (progn
          (overleaf-project--git-run repo (list "read-tree" revision) env)
          (overleaf-project--git-run
           repo
           (list
            "checkout-index"
            "-a"
            "-f"
            (format "--prefix=%s/" (file-name-as-directory temp-dir)))
           env)
          temp-dir)
      (ignore-errors (delete-file index-file)))))

(defun overleaf-project--commit-directory (repo directory parent message)
  "Create a Git commit in REPO from DIRECTORY with PARENT and MESSAGE.
Return the created commit id."
  (let* ((index-file (overleaf-project--make-temp-index-path))
         (env (list (concat "GIT_INDEX_FILE=" index-file)))
         (tree nil))
    (unwind-protect
        (progn
          (overleaf-project--git-run
           repo
           (list
            "--git-dir" (expand-file-name ".git" repo)
            "--work-tree" directory
            "add" "--all" ".")
           env)
          (setq tree
                (overleaf-project--command-result-output
                 (overleaf-project--git-run repo (list "write-tree") env)))
          (overleaf-project--command-result-output
           (overleaf-project--git-run
            repo
            (append
             (overleaf-project--git-identity-args repo)
             (list "commit-tree" tree)
             (when parent (list "-p" parent))
             (list "-m" message))
            env)))
      (ignore-errors (delete-file index-file)))))

(defun overleaf-project--git-identity-args (repo)
  "Return fallback Git identity args for REPO when necessary."
  (if (and (overleaf-project--git-output-noerror repo "config" "--get" "user.name")
           (overleaf-project--git-output-noerror repo "config" "--get" "user.email"))
      nil
    (overleaf-project--warn
     "Git identity is not configured for %s; using a repository-local placeholder author"
     repo)
    '("-c" "user.name=Overleaf Project"
      "-c" "user.email=overleaf-project@local")))

(defun overleaf-project--commit-working-tree (repo)
  "Commit staged changes in REPO before pushing."
  (apply
   #'overleaf-project--git-output
   repo
   (append
    (overleaf-project--git-identity-args repo)
    (if (overleaf-project--merge-in-progress-p repo)
        '("commit" "--no-edit")
      (list "commit" "-m" overleaf-project-sync-auto-commit-message)))))

(defun overleaf-project--prepare-working-tree-for-sync
    (repo &optional unstaged-action)
  "Stage and commit local changes in REPO when needed for pushing.

UNSTAGED-ACTION controls how unstaged or untracked changes are handled:
nil or `prompt' asks before staging all changes, `stage' stages all
changes without prompting, and `error' signals a user error."
  (let ((status (overleaf-project--read-repo-status repo)))
    (when (overleaf-project--repo-status-unmerged status)
      (user-error
       "Repository %s has unresolved merge conflicts; resolve them before pushing"
       repo))
    (when (overleaf-project--repo-status-unstaged status)
      (pcase (or unstaged-action 'prompt)
        ('stage nil)
        ('error
         (user-error
          "Overleaf push requires a clean working tree; stage all changes or stash them first"))
        (_
         (unless
             (y-or-n-p
              (format
               "Repository %s has unstaged changes. Stage all changes and continue with Overleaf push? "
               repo))
           (user-error
            "Overleaf push requires a clean working tree; stage all changes or stash them first"))))
      (overleaf-project--git-output repo "add" "--all" ".")
      (setq status (overleaf-project--read-repo-status repo)))
    (when (overleaf-project--repo-status-staged status)
      (overleaf-project--create-local-backup-ref repo "before-auto-commit")
      (overleaf-project--message "Committing local changes before Overleaf push...")
      (overleaf-project--commit-working-tree repo)
      t)))

(defun overleaf-project--ensure-clean-working-tree (repo action)
  "Signal an error if REPO has local changes before ACTION."
  (let ((status (overleaf-project--read-repo-status repo)))
    (when (overleaf-project--repo-status-unmerged status)
      (user-error
       "Repository %s has unresolved merge conflicts; resolve them before %s"
       repo
       action))
    (when (or (overleaf-project--repo-status-staged status)
              (overleaf-project--repo-status-unstaged status))
      (user-error
       "Repository %s has local changes; commit or stash them before %s"
       repo
       action))))

;;;; Project sync internals

(defun overleaf-project--sync-local-tree
    (project-id local-root remote-root remote-table)
  "Synchronize LOCAL-ROOT into PROJECT-ID using REMOTE-ROOT and REMOTE-TABLE."
  (let* ((local-state (overleaf-project--scan-local-tree local-root))
         (local-dirs (plist-get local-state :dirs))
         (local-files (plist-get local-state :files))
         (dir-paths nil)
         (file-paths nil)
         (delete-files nil)
         (delete-folders nil))
    (maphash
     (lambda (path _)
       (unless (string-empty-p path)
         (push path dir-paths)))
     local-dirs)
    (maphash
     (lambda (path _)
       (push path file-paths))
     local-files)

    (dolist (path
             (sort dir-paths
                   (lambda (left right)
                     (< (overleaf-project--path-depth left)
                        (overleaf-project--path-depth right)))))
      (let ((remote-entry (gethash path remote-table)))
        (when remote-entry
          (unless (eq (overleaf-project--entity-type remote-entry) 'folder)
            (overleaf-project--delete-entity project-id remote-entry)
            (overleaf-project--forget-entry remote-table path)
            (setq remote-entry nil)))
        (unless remote-entry
          (let* ((parent-path (overleaf-project--parent-path path))
                 (parent-entry (gethash parent-path remote-table))
                 (created
                  (overleaf-project--create-folder
                   project-id
                   (overleaf-project--entity-id parent-entry)
                   (file-name-nondirectory path))))
            (puthash
             path
             (make-overleaf-project--entity
              :path path
              :name (plist-get created :name)
              :id (plist-get created :_id)
              :type 'folder
              :parent-id (overleaf-project--entity-id parent-entry))
             remote-table)))))

    (dolist (path (sort file-paths #'string<))
      (let* ((local-file (gethash path local-files))
             (remote-entry (gethash path remote-table))
             (remote-file (expand-file-name path remote-root))
             (same-content
              (and remote-entry
                   (not (eq (overleaf-project--entity-type remote-entry) 'folder))
                   (overleaf-project--files-equal-p local-file remote-file))))
        (unless same-content
          (when remote-entry
            (overleaf-project--delete-entity project-id remote-entry)
            (overleaf-project--forget-entry remote-table path))
          (let* ((parent-path (overleaf-project--parent-path path))
                 (parent-entry (gethash parent-path remote-table))
                 (response
                  (overleaf-project--curl-upload-file
                   project-id
                   (overleaf-project--entity-id parent-entry)
                   (file-name-nondirectory path)
                   local-file))
                 (entity-type
                  (pcase (plist-get response :entity_type)
                    ("doc" 'doc)
                    (_ 'file))))
            (puthash
             path
             (make-overleaf-project--entity
              :path path
              :name (file-name-nondirectory path)
              :id (plist-get response :entity_id)
              :type entity-type
              :parent-id (overleaf-project--entity-id parent-entry))
             remote-table)))))

    (maphash
     (lambda (path entity)
       (unless (string-empty-p path)
         (unless (or (overleaf-project--sync-metadata-path-p path)
                     (gethash path local-files)
                     (gethash path local-dirs))
           (if (eq (overleaf-project--entity-type entity) 'folder)
               (push path delete-folders)
             (push path delete-files)))))
     remote-table)

    (dolist (path
             (sort delete-files
                   (lambda (left right)
                     (> (overleaf-project--path-depth left)
                        (overleaf-project--path-depth right)))))
      (when-let ((entity (gethash path remote-table)))
        (overleaf-project--delete-entity project-id entity)
        (remhash path remote-table)))

    (dolist (path
             (sort delete-folders
                   (lambda (left right)
                     (> (overleaf-project--path-depth left)
                        (overleaf-project--path-depth right)))))
      (when-let ((entity (gethash path remote-table)))
        (overleaf-project--delete-entity project-id entity)
        (overleaf-project--forget-entry remote-table path)))))

(defun overleaf-project--upload-sync-metadata
    (repo revision project-id remote-table)
  "Upload sync metadata for REVISION in REPO to PROJECT-ID."
  (when overleaf-project-sync-metadata-enabled
    (condition-case err
        (let* ((path (overleaf-project--sync-metadata-relative-path))
               (root-entry (gethash "" remote-table))
	       (temp-file (make-temp-file "overleaf-project-sync-metadata." nil ".json")))
          (unless root-entry
            (user-error "Could not find remote Overleaf root folder"))
          (unwind-protect
              (progn
                (with-temp-file temp-file
                  (insert
                   (overleaf-project--sync-metadata-json
                    repo
                    revision
                    project-id)))
                (when-let ((existing (gethash path remote-table)))
                  (overleaf-project--delete-entity project-id existing)
                  (overleaf-project--forget-entry remote-table path))
                (let* ((response
                        (overleaf-project--curl-upload-file
                         project-id
                         (overleaf-project--entity-id root-entry)
                         path
                         temp-file))
                       (entity-type
                        (pcase (plist-get response :entity_type)
                          ("doc" 'doc)
                          (_ 'file))))
                  (puthash
                   path
                   (make-overleaf-project--entity
                    :path path
                    :name path
                    :id (plist-get response :entity_id)
                    :type entity-type
                    :parent-id (overleaf-project--entity-id root-entry))
                   remote-table)))
            (ignore-errors (delete-file temp-file))))
      (error
       (overleaf-project--warn
        "Could not update remote Overleaf sync metadata: %s"
        (error-message-string err))))))

(defun overleaf-project--sync-commit
    (repo revision project-id remote-root remote-table)
  "Synchronize REVISION from REPO into PROJECT-ID."
  (let ((local-root nil))
    (unwind-protect
        (progn
          (setq local-root (overleaf-project--materialize-commit repo revision))
          (overleaf-project--message "Uploading %s to Overleaf..." revision)
          (overleaf-project--sync-local-tree
           project-id local-root remote-root remote-table))
      (when local-root
        (ignore-errors (delete-directory local-root t))))))

(defun overleaf-project--record-remote-snapshot (repo remote-root)
  "Create a Git commit in REPO representing REMOTE-ROOT."
  (let* ((snapshot-commit
          (overleaf-project--commit-directory
           repo
           remote-root
           (overleaf-project--rev-parse-noerror
            repo
            (overleaf-project--base-ref repo))
           (format "overleaf: remote snapshot %s"
                   (format-time-string "%Y-%m-%d %H:%M:%S"))))
         (snapshot-tree (overleaf-project--tree-id repo snapshot-commit)))
    (or (overleaf-project--remote-sync-metadata-commit repo snapshot-tree)
        snapshot-commit)))

(defun overleaf-project--initialize-base-ref (repo project remote-root)
  "Persist PROJECT in REPO and initialize the hidden Overleaf base ref.
REMOTE-ROOT must point at a downloaded snapshot of PROJECT.  This does
not modify the working tree or perform a pull/push."
  (let ((remote-commit
         (overleaf-project--commit-directory
          repo
          remote-root
          (overleaf-project--rev-parse-noerror
           repo
           (overleaf-project--base-ref repo))
          (format "overleaf: configured base snapshot %s"
                  (format-time-string "%Y-%m-%d %H:%M:%S")))))
    (overleaf-project--write-repo-metadata repo project)
    (overleaf-project--clear-pending-state repo)
    (overleaf-project--set-base-ref repo remote-commit)
    remote-commit))

(defun overleaf-project--classify-sync-state (base-tree head-tree remote-tree)
  "Classify the sync relationship between BASE-TREE, HEAD-TREE, and REMOTE-TREE."
  (cond
   ((and (string= head-tree base-tree)
         (string= remote-tree base-tree))
    'in-sync)
   ((string= head-tree remote-tree)
    'head-matches-remote)
   ((string= remote-tree base-tree)
    'remote-matches-base)
   ((string= head-tree base-tree)
    'head-matches-base)
   (t
    'diverged)))

(defun overleaf-project--read-sync-state (repo remote-root)
  "Return common sync state for REPO against REMOTE-ROOT."
  (let* ((base-ref (overleaf-project--base-ref repo))
         (base-commit (overleaf-project--rev-parse repo base-ref))
         (head (overleaf-project--rev-parse repo "HEAD"))
         (branch (overleaf-project--current-branch repo))
         (remote-commit (overleaf-project--record-remote-snapshot repo remote-root))
         (base-tree (overleaf-project--tree-id repo base-commit))
         (head-tree (overleaf-project--tree-id repo head))
         (remote-tree (overleaf-project--tree-id repo remote-commit)))
    `(:base-commit ,base-commit
		   :head ,head
		   :branch ,branch
		   :remote-commit ,remote-commit
		   :base-tree ,base-tree
		   :head-tree ,head-tree
		   :remote-tree ,remote-tree
		   :status ,(overleaf-project--classify-sync-state
			     base-tree head-tree remote-tree))))

(defun overleaf-project--ensure-pending-action (repo pending action)
  "Validate that PENDING in REPO matches ACTION."
  (when pending
    (let* ((pending-action (or (plist-get pending :action) 'push))
           (pending-command (format "`overleaf-project-%s`"
                                    (symbol-name pending-action)))
           (sync-branch (plist-get pending :sync-branch)))
      (when (and sync-branch
                 (not (string= (overleaf-project--current-branch repo) sync-branch)))
        (user-error
         "Pending Overleaf %s exists on branch `%s'; checkout that branch and rerun %s"
         pending-action
         sync-branch
         pending-command))
      (unless (eq pending-action action)
        (user-error
         "Pending Overleaf %s exists; rerun %s"
         pending-action
         pending-command)))))

(defun overleaf-project--ensure-no-pending-action (repo command)
  "Signal if REPO still has a pending Overleaf sync before COMMAND."
  (when-let* ((pending (overleaf-project--pending-state repo)))
    (let ((action (or (plist-get pending :action) 'push))
          (sync-branch (plist-get pending :sync-branch)))
      (if sync-branch
          (user-error
           "Pending Overleaf %s exists on branch `%s'; finish it before %s"
           action sync-branch command)
        (user-error
         "Pending Overleaf %s exists; finish it before %s"
         action command)))))

(defun overleaf-project--validate-pending-sync (repo pending action)
  "Validate PENDING sync metadata for REPO and ACTION.
Return a plist containing the common pending-sync state."
  (let* ((command (format "`overleaf-project-%s`" (symbol-name action)))
         (head (overleaf-project--rev-parse repo "HEAD"))
         (remote-commit (plist-get pending :remote-commit))
         (original-head (plist-get pending :original-head))
         (original-branch (plist-get pending :original-branch))
         (remote-tree (and remote-commit
                           (overleaf-project--tree-id repo remote-commit))))
    (when (overleaf-project--merge-in-progress-p repo)
      (user-error
       "Merge is still in progress on %s; finish the merge commit and rerun %s"
       (plist-get pending :sync-branch)
       command))
    (unless (and original-head original-branch remote-commit)
      (user-error "Pending Overleaf %s metadata is incomplete" action))
    (unless (overleaf-project--is-ancestor-p repo original-head head)
      (user-error "Current branch no longer descends from the original branch head"))
    (unless (overleaf-project--is-ancestor-p repo remote-commit head)
      (user-error "Current branch no longer contains the downloaded remote snapshot"))
    `(:head ,head
	    :remote-commit ,remote-commit
	    :original-head ,original-head
	    :original-branch ,original-branch
	    :remote-tree ,remote-tree)))

(defun overleaf-project--ensure-pending-remote-unchanged
    (repo remote-root remote-tree action)
  "Signal if REMOTE-ROOT no longer matches REMOTE-TREE for pending ACTION."
  (let ((current-remote-commit
         (overleaf-project--record-remote-snapshot repo remote-root)))
    (unless (string=
             (overleaf-project--tree-id repo current-remote-commit)
             remote-tree)
      (user-error
       "The remote project changed again while the %s branch was pending; start a new %s"
       action
       action))
    current-remote-commit))

(defun overleaf-project--finish-pending-sync
    (repo action original-branch original-head head format-string &rest args)
  "Finish a pending Overleaf ACTION for REPO and restore ORIGINAL-BRANCH.
ORIGINAL-HEAD is the expected previous branch tip, HEAD is the new tip,
and FORMAT-STRING with ARGS is passed to `overleaf-project--message'."
  (overleaf-project--create-local-backup-ref
   repo
   (format "%s-original-branch" action)
   original-head)
  (overleaf-project--create-local-backup-ref
   repo
   (format "%s-finished-head" action)
   head)
  (let ((update-result
         (overleaf-project--git-run
          repo
          (list "update-ref"
                (format "refs/heads/%s" original-branch)
                head
                original-head)
          nil
          t)))
    (unless (and (integerp (overleaf-project--command-result-status update-result))
                 (zerop (overleaf-project--command-result-status update-result)))
      (error
       "Overleaf %s finished, but could not move branch `%s`: %s"
       action
       original-branch
       (overleaf-project--command-result-output update-result))))
  (overleaf-project--git-output repo "checkout" original-branch)
  (overleaf-project--clear-pending-state repo)
  (apply #'overleaf-project--message format-string args))

(defun overleaf-project--note-matching-sync-state
    (repo head &optional project-id remote-table)
  "Update REPO base metadata after confirming HEAD already matches Overleaf.
When PROJECT-ID and REMOTE-TABLE are non-nil, also refresh remote sync
metadata."
  (when (and project-id remote-table)
    (overleaf-project--upload-sync-metadata repo head project-id remote-table))
  (overleaf-project--set-base-ref repo head)
  (overleaf-project--message "Local and remote content already match; base ref updated"))

(defun overleaf-project--upload-head-and-set-base
    (repo head project-id remote-root remote-table format-string &rest args)
  "Upload HEAD from REPO to Overleaf, update the base ref, and report success."
  (overleaf-project--sync-commit
   repo head project-id remote-root remote-table)
  (overleaf-project--upload-sync-metadata repo head project-id remote-table)
  (overleaf-project--set-base-ref repo head)
  (apply #'overleaf-project--message format-string args))

(defun overleaf-project--finalize-pending-pull
    (repo pending remote-root remote-table)
  "Finalize a pending pull in REPO: upload merged HEAD to Overleaf.
PENDING must have action=pull and a valid remote-commit.
REMOTE-ROOT and REMOTE-TABLE describe the current remote state."
  (let* ((remote-commit (plist-get pending :remote-commit)))
    (unless remote-commit
      (user-error "Pending pull metadata is incomplete"))
    (let* ((head (overleaf-project--rev-parse repo "HEAD"))
           (project-id (overleaf-project--project-id repo))
           (remote-tree (overleaf-project--tree-id repo remote-commit)))
      (unless (overleaf-project--is-ancestor-p repo remote-commit head)
        (user-error
         "Merge is not complete; resolve conflicts and commit before pushing"))
      (overleaf-project--ensure-pending-remote-unchanged
       repo remote-root remote-tree 'pull)
      (overleaf-project--upload-head-and-set-base
       repo head project-id remote-root remote-table
       "Pushed merged Overleaf pull for `%s'"
       (overleaf-project--project-name repo))
      (overleaf-project--clear-pending-state repo))))

(defun overleaf-project--finalize-pending-push
    (repo pending remote-root remote-table)
  "Finalize PENDING push in REPO using REMOTE-ROOT and REMOTE-TABLE."
  (let* ((context (overleaf-project--validate-pending-sync repo pending 'push))
         (head (plist-get context :head))
         (head-tree (overleaf-project--tree-id repo head))
         (project-id (overleaf-project--project-id repo))
         (original-head (plist-get context :original-head))
         (original-branch (plist-get context :original-branch))
         (remote-tree (plist-get context :remote-tree))
         (uploaded nil))
    (overleaf-project--ensure-pending-remote-unchanged
     repo remote-root remote-tree 'push)
    (unless (string= head-tree remote-tree)
      (overleaf-project--sync-commit
       repo
       head
       project-id
       remote-root
       remote-table)
      (setq uploaded t))
    (overleaf-project--upload-sync-metadata repo head project-id remote-table)
    (overleaf-project--set-base-ref repo head)
    (overleaf-project--finish-pending-sync
     repo
     'push
     original-branch
     original-head
     head
     (if uploaded
         "Pushed merged changes for `%s' and updated branch `%s'"
       "Overleaf already matched the merged branch for `%s'; updated branch `%s'")
     (overleaf-project--project-name repo)
     original-branch)))

(defun overleaf-project--fresh-push (repo remote-root remote-table)
  "Perform a fresh push of REPO using REMOTE-ROOT and REMOTE-TABLE."
  (let* ((context (overleaf-project--read-sync-state repo remote-root))
         (head (plist-get context :head))
         (branch (plist-get context :branch))
         (project-id (overleaf-project--project-id repo))
         (status (plist-get context :status)))
    (pcase status
      ('in-sync
       (overleaf-project--upload-sync-metadata repo head project-id remote-table)
       (overleaf-project--message "Project `%s' is already in sync"
				  (overleaf-project--project-name repo)))
      ('head-matches-remote
       (overleaf-project--note-matching-sync-state
        repo
        head
        project-id
        remote-table))
      ('remote-matches-base
       (overleaf-project--upload-head-and-set-base
        repo
        head
        project-id
        remote-root
        remote-table
        "Pushed `%s' to Overleaf"
        (overleaf-project--project-name repo)))
      ('head-matches-base
       (user-error
        "Remote Overleaf changes exist for `%s'; run `overleaf-project-pull` first"
        branch))
      (_
       (user-error
        "Remote Overleaf changes exist for `%s'; run `overleaf-project-pull' first"
        (overleaf-project--project-name repo))))))

(defun overleaf-project--fresh-pull (repo remote-root)
  "Perform a fresh pull of REPO using REMOTE-ROOT."
  (let* ((context (overleaf-project--read-sync-state repo remote-root))
         (head (plist-get context :head))
         (branch (plist-get context :branch))
         (remote-commit (plist-get context :remote-commit))
         (status (plist-get context :status)))
    (pcase status
      ('in-sync
       (overleaf-project--message "Project `%s' is already in sync"
				  (overleaf-project--project-name repo)))
      ('head-matches-remote
       (overleaf-project--note-matching-sync-state repo head))
      ('remote-matches-base
       (overleaf-project--message "No remote Overleaf changes to pull into `%s'" branch))
      ('head-matches-base
       (overleaf-project--create-local-backup-ref repo "pull-ff")
       (overleaf-project--git-output repo "merge" "--ff-only" remote-commit)
       (overleaf-project--set-base-ref repo "HEAD")
       (overleaf-project--message "Pulled remote Overleaf changes into `%s'" branch))
      (_
       (overleaf-project--create-local-backup-ref repo "pull-merge")
       (let ((merge-result
              (overleaf-project--git-run
               repo
               (list "merge" "--no-ff" "--no-edit" remote-commit)
               nil
               t)))
         (if (and (integerp (overleaf-project--command-result-status merge-result))
                  (zerop (overleaf-project--command-result-status merge-result)))
             (progn
               (overleaf-project--set-base-ref repo remote-commit)
               (overleaf-project--message "Pulled Overleaf changes into `%s'" branch))
           (overleaf-project--create-local-backup-ref
            repo
            "pending-pull-remote"
            remote-commit)
           (overleaf-project--set-pending-pull-state repo remote-commit)
           (overleaf-project--warn
            "Merge conflict on `%s'. Resolve conflicts, commit, then run `overleaf-project-push'."
            branch)))))))


(provide 'overleaf-project-sync)

;;; overleaf-project-sync.el ends here
