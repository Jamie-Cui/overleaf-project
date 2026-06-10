;;; overleaf-project-git-test.el --- Git integration tests for overleaf-project -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'overleaf-project-core)
(require 'overleaf-project-http)
(require 'overleaf-project-sync)

(defmacro overleaf-project-git-test--with-temp-dir (var &rest body)
  "Bind VAR to a temporary directory while running BODY."
  (declare (indent 1) (debug t))
  `(let ((,var (make-temp-file "overleaf-project-git-test." t)))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-directory ,var t)))))

(defmacro overleaf-project-git-test--with-repo (var &rest body)
  "Bind VAR to a temporary Git repository while running BODY."
  (declare (indent 1) (debug t))
  `(let ((,var (overleaf-project-git-test--init-repo)))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-directory ,var t)))))

(defun overleaf-project-git-test--ensure-git ()
  "Skip the current test when Git is unavailable."
  (unless (executable-find overleaf-project-git-executable)
    (ert-skip "Git executable is unavailable")))

(defun overleaf-project-git-test--git (repo &rest args)
  "Run Git ARGS in REPO and return stdout."
  (apply #'overleaf-project--git-output repo args))

(defun overleaf-project-git-test--init-repo ()
  "Create and return a temporary Git repository."
  (overleaf-project-git-test--ensure-git)
  (let ((repo (make-temp-file "overleaf-project-repo." t)))
    (overleaf-project--run overleaf-project-git-executable '("init") repo)
    (overleaf-project-git-test--git repo "config" "user.name" "Overleaf Test")
    (overleaf-project-git-test--git
     repo "config" "user.email" "overleaf-test@example.invalid")
    repo))

(defun overleaf-project-git-test--write-file (root relative text)
  "Write TEXT to RELATIVE under ROOT and return the absolute path."
  (let ((file (expand-file-name relative root)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert text))
    file))

(defun overleaf-project-git-test--read-file (root relative)
  "Return the contents of RELATIVE under ROOT."
  (with-temp-buffer
    (insert-file-contents (expand-file-name relative root))
    (buffer-string)))

(defun overleaf-project-git-test--commit-all (repo message)
  "Commit all changes in REPO with MESSAGE and return HEAD."
  (overleaf-project-git-test--git repo "add" "--all" ".")
  (overleaf-project-git-test--git repo "commit" "-m" message)
  (overleaf-project-git-test--git repo "rev-parse" "HEAD"))

(defun overleaf-project-git-test--remote-table ()
  "Return a minimal remote entity table with a root folder."
  (let ((table (make-hash-table :test #'equal)))
    (puthash
     ""
     (make-overleaf-project--entity
      :path ""
      :name "rootFolder"
      :id "root"
      :type 'folder)
     table)
    table))

(defun overleaf-project-git-test--mark-managed (repo)
  "Configure REPO as a managed Overleaf repository."
  (overleaf-project--git-config-set repo "overleaf-project.projectId" "project-id")
  (overleaf-project--git-config-set repo "overleaf-project.projectName" "Project")
  (overleaf-project--git-config-set
   repo "overleaf-project.url" "https://www.overleaf.com")
  (overleaf-project--git-config-set
   repo "overleaf-project.baseRef" overleaf-project-base-ref))

(defun overleaf-project-git-test--base-commit (repo text)
  "Create a base commit in REPO containing TEXT and set the base ref."
  (overleaf-project-git-test--write-file repo "main.tex" text)
  (let ((commit (overleaf-project-git-test--commit-all repo "base")))
    (overleaf-project-git-test--mark-managed repo)
    (overleaf-project--set-base-ref repo commit)
    commit))

(defmacro overleaf-project-git-test--without-remote-side-effects (&rest body)
  "Run BODY with Overleaf network side effects stubbed."
  (declare (indent 0) (debug t))
  `(let ((sync-calls nil)
         (metadata-calls nil)
         (messages nil)
         (warnings nil))
     (cl-letf (((symbol-function 'overleaf-project--sync-commit)
                (lambda (&rest args)
                  (push args sync-calls)))
               ((symbol-function 'overleaf-project--upload-sync-metadata)
                (lambda (&rest args)
                  (push args metadata-calls)))
               ((symbol-function 'overleaf-project--message)
                (lambda (&rest args)
                  (push args messages)))
               ((symbol-function 'overleaf-project--warn)
                (lambda (&rest args)
                  (push args warnings))))
       ,@body)))

(ert-deftest overleaf-project-git-test-commit-directory-and-materialize ()
  (overleaf-project-git-test--with-repo repo
    (overleaf-project-git-test--write-file repo "base.tex" "base\n")
    (let ((parent (overleaf-project-git-test--commit-all repo "base"))
          (materialized nil))
      (overleaf-project-git-test--with-temp-dir snapshot
        (overleaf-project-git-test--write-file snapshot "main.tex" "remote\n")
        (overleaf-project-git-test--write-file
         snapshot
         "chapters/intro.tex"
         "intro\n")
        (let ((commit (overleaf-project--commit-directory
                       repo
                       snapshot
                       parent
                       "remote snapshot")))
          (should (string-match-p "\\`[[:xdigit:]]\\{40,64\\}\\'" commit))
          (should (string-match-p
                   parent
                   (overleaf-project-git-test--git
                    repo
                    "rev-list"
                    "--parents"
                    "-n"
                    "1"
                    commit)))
          (unwind-protect
              (progn
                (setq materialized
                      (overleaf-project--materialize-commit repo commit))
                (should (equal (overleaf-project-git-test--read-file
                                materialized
                                "main.tex")
                               "remote\n"))
                (should (equal (overleaf-project-git-test--read-file
                                materialized
                                "chapters/intro.tex")
                               "intro\n"))
                (should-not (file-exists-p
                             (expand-file-name "base.tex" materialized))))
            (when materialized
              (ignore-errors (delete-directory materialized t)))))))))

(ert-deftest overleaf-project-git-test-initialize-base-ref-writes-metadata ()
  (let ((overleaf-project-url "https://example.overleaf.test")
        (overleaf-project-log-echo nil))
    (overleaf-project-git-test--with-repo repo
      (overleaf-project-git-test--write-file repo "local.tex" "local\n")
      (overleaf-project-git-test--commit-all repo "local")
      (overleaf-project--set-pending-pull-state repo "pending")
      (overleaf-project-git-test--with-temp-dir snapshot
        (overleaf-project-git-test--write-file snapshot "remote.tex" "remote\n")
        (let ((commit (overleaf-project--initialize-base-ref
                       repo
                       '(:id "project-id" :name "Project")
                       snapshot))
              (materialized nil))
          (should (equal (overleaf-project--git-config-get
                          repo
                          "overleaf-project.projectId")
                         "project-id"))
          (should (equal (overleaf-project--git-config-get
                          repo
                          "overleaf-project.projectName")
                         "Project"))
          (should (equal (overleaf-project--git-config-get
                          repo
                          "overleaf-project.url")
                         "https://example.overleaf.test"))
          (should-not (overleaf-project--pending-state repo))
          (should (equal (overleaf-project--rev-parse
                          repo
                          (overleaf-project--base-ref repo))
                         commit))
          (unwind-protect
              (progn
                (setq materialized
                      (overleaf-project--materialize-commit repo commit))
                (should (equal (overleaf-project-git-test--read-file
                                materialized
                                "remote.tex")
                               "remote\n"))
                (should-not (file-exists-p
                             (expand-file-name "local.tex" materialized))))
            (when materialized
              (ignore-errors (delete-directory materialized t)))))))))

(ert-deftest overleaf-project-git-test-record-remote-snapshot-uses-metadata ()
  (let ((overleaf-project-log-echo nil))
    (overleaf-project-git-test--with-repo repo
      (overleaf-project-git-test--write-file repo "main.tex" "same\n")
      (let* ((head (overleaf-project-git-test--commit-all repo "local"))
             (tree (overleaf-project--tree-id repo head))
             (overleaf-project--remote-sync-metadata
              `(:localCommit ,head :localTree ,tree))
             (overleaf-project-sync-metadata-enabled t))
        (overleaf-project-git-test--with-temp-dir snapshot
          (overleaf-project-git-test--write-file snapshot "main.tex" "same\n")
          (should (equal (overleaf-project--record-remote-snapshot
                          repo
                          snapshot)
                         head)))))))

(ert-deftest overleaf-project-git-test-fresh-push-uploads-local-head ()
  (let ((overleaf-project-log-echo nil))
    (overleaf-project-git-test--with-repo repo
      (overleaf-project-git-test--base-commit repo "base\n")
      (overleaf-project-git-test--write-file repo "main.tex" "local\n")
      (let ((head (overleaf-project-git-test--commit-all repo "local")))
        (overleaf-project-git-test--with-temp-dir remote-root
          (overleaf-project-git-test--write-file remote-root "main.tex" "base\n")
          (overleaf-project-git-test--without-remote-side-effects
            (overleaf-project--fresh-push
             repo
             remote-root
             (overleaf-project-git-test--remote-table))
            (should (= (length sync-calls) 1))
            (should (= (length metadata-calls) 1))
            (should (equal (overleaf-project--rev-parse
                            repo
                            (overleaf-project--base-ref repo))
                           head))))))))

(ert-deftest overleaf-project-git-test-fresh-push-rejects-remote-changes ()
  (let ((overleaf-project-log-echo nil))
    (overleaf-project-git-test--with-repo repo
      (overleaf-project-git-test--base-commit repo "base\n")
      (overleaf-project-git-test--with-temp-dir remote-root
        (overleaf-project-git-test--write-file remote-root "main.tex" "remote\n")
        (overleaf-project-git-test--without-remote-side-effects
          (should-error
           (overleaf-project--fresh-push
            repo
            remote-root
            (overleaf-project-git-test--remote-table))
           :type 'user-error)
          (should-not sync-calls)
          (should-not metadata-calls))))
    (overleaf-project-git-test--with-repo repo
      (overleaf-project-git-test--base-commit repo "base\n")
      (overleaf-project-git-test--write-file repo "main.tex" "local\n")
      (overleaf-project-git-test--commit-all repo "local")
      (overleaf-project-git-test--with-temp-dir remote-root
        (overleaf-project-git-test--write-file remote-root "main.tex" "remote\n")
        (overleaf-project-git-test--without-remote-side-effects
          (should-error
           (overleaf-project--fresh-push
            repo
            remote-root
            (overleaf-project-git-test--remote-table))
           :type 'user-error)
          (should-not sync-calls)
          (should-not metadata-calls))))))

(ert-deftest overleaf-project-git-test-fresh-pull-fast-forwards ()
  (let ((overleaf-project-log-echo nil)
        (overleaf-project-local-backups-enabled t))
    (overleaf-project-git-test--with-repo repo
      (overleaf-project-git-test--base-commit repo "base\n")
      (overleaf-project-git-test--with-temp-dir remote-root
        (overleaf-project-git-test--write-file remote-root "main.tex" "remote\n")
        (overleaf-project--fresh-pull repo remote-root)
        (should (equal (overleaf-project-git-test--read-file
                        repo
                        "main.tex")
                       "remote\n"))
        (should (equal (overleaf-project--rev-parse
                        repo
                        (overleaf-project--base-ref repo))
                       (overleaf-project--rev-parse repo "HEAD")))
        (should-not (overleaf-project--pending-state repo))
        (should-not (string-empty-p
                     (overleaf-project-git-test--git
                      repo
                      "for-each-ref"
                      "--format=%(refname)"
                      "refs/overleaf-project/backups")))))))

(ert-deftest overleaf-project-git-test-fresh-pull-records-pending-conflict ()
  (let ((overleaf-project-log-echo nil)
        (overleaf-project-local-backups-enabled t))
    (overleaf-project-git-test--with-repo repo
      (overleaf-project-git-test--base-commit repo "base\n")
      (overleaf-project-git-test--write-file repo "main.tex" "local\n")
      (overleaf-project-git-test--commit-all repo "local")
      (overleaf-project-git-test--with-temp-dir remote-root
        (overleaf-project-git-test--write-file remote-root "main.tex" "remote\n")
        (overleaf-project--fresh-pull repo remote-root)
        (let ((pending (overleaf-project--pending-state repo))
              (status (overleaf-project--read-repo-status repo)))
          (should (equal (plist-get pending :action) 'pull))
          (should (overleaf-project--git-object-id-p
                   (plist-get pending :remote-commit)))
          (should (overleaf-project--merge-in-progress-p repo))
          (should (overleaf-project--repo-status-unmerged status)))
        (should-not (string-empty-p
                     (overleaf-project-git-test--git
                      repo
                      "for-each-ref"
                      "--format=%(refname)"
                      "refs/overleaf-project/backups")))))))

(ert-deftest overleaf-project-git-test-working-tree-error-branches ()
  (let ((overleaf-project-log-echo nil))
    (overleaf-project-git-test--with-repo repo
      (overleaf-project-git-test--write-file repo "base.tex" "base\n")
      (overleaf-project-git-test--commit-all repo "base")
      (should-error (overleaf-project--require-managed-repo repo)
                    :type 'user-error))
    (overleaf-project-git-test--with-repo repo
      (overleaf-project-git-test--base-commit repo "base\n")
      (overleaf-project-git-test--write-file repo "dirty.tex" "dirty\n")
      (should-error (overleaf-project--ensure-clean-working-tree
                     repo
                     "testing")
                    :type 'user-error)
      (should-error (overleaf-project--prepare-working-tree-for-sync
                     repo
                     'error)
                    :type 'user-error)
      (overleaf-project-git-test--git repo "add" "dirty.tex")
      (should-error (overleaf-project--ensure-clean-working-tree
                     repo
                     "testing")
                    :type 'user-error))
    (overleaf-project-git-test--with-repo repo
      (overleaf-project-git-test--base-commit repo "base\n")
      (overleaf-project-git-test--git repo "checkout" "--detach" "HEAD")
      (should-error (overleaf-project--current-branch repo)
                    :type 'user-error))))

;;; overleaf-project-git-test.el ends here
