;;; overleaf-project-sync-tree-test.el --- Remote tree sync tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'overleaf-project-core)
(require 'overleaf-project-http)
(require 'overleaf-project-sync)

(defmacro overleaf-project-sync-tree-test--with-temp-dir (var &rest body)
  "Bind VAR to a temporary directory while running BODY."
  (declare (indent 1) (debug t))
  `(let ((,var (make-temp-file "overleaf-project-sync-tree-test." t)))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-directory ,var t)))))

(defun overleaf-project-sync-tree-test--write-file (root relative text)
  "Write TEXT to RELATIVE under ROOT and return the absolute path."
  (let ((file (expand-file-name relative root)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert text))
    file))

(defun overleaf-project-sync-tree-test--read-file (file)
  "Return FILE contents as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun overleaf-project-sync-tree-test--root-table ()
  "Return a remote table containing only the root folder."
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

(defun overleaf-project-sync-tree-test--put-entity
    (table path id type &optional parent-id)
  "Put a remote entity in TABLE at PATH with ID, TYPE, and PARENT-ID."
  (puthash
   path
   (make-overleaf-project--entity
    :path path
    :name (file-name-nondirectory path)
    :id id
    :type type
    :parent-id (or parent-id "root"))
   table))

(defmacro overleaf-project-sync-tree-test--with-remote-stubs (&rest body)
  "Run BODY with Overleaf remote mutation helpers stubbed."
  (declare (indent 0) (debug t))
  `(let ((events nil)
         (folder-counter 0)
         (upload-counter 0))
     (cl-letf (((symbol-function 'overleaf-project--create-folder)
                (lambda (project-id parent-id name)
                  (setq folder-counter (1+ folder-counter))
                  (let ((id (format "created-folder-%d" folder-counter)))
                    (push `(:create-folder ,project-id ,parent-id ,name ,id)
                          events)
                    `(:_id ,id :name ,name))))
               ((symbol-function 'overleaf-project--delete-entity)
                (lambda (project-id entity)
                  (push `(:delete
                          ,project-id
                          ,(overleaf-project--entity-path entity)
                          ,(overleaf-project--entity-id entity)
                          ,(overleaf-project--entity-type entity))
                        events)))
               ((symbol-function 'overleaf-project--curl-upload-file)
                (lambda (project-id folder-id file-name file-path)
                  (setq upload-counter (1+ upload-counter))
                  (let ((id (format "uploaded-%d" upload-counter)))
                    (push `(:upload
                            ,project-id
                            ,folder-id
                            ,file-name
                            ,(overleaf-project-sync-tree-test--read-file
                              file-path)
                            ,id)
                          events)
                    `(:entity_id
                      ,id
                      :entity_type
                      ,(if (string-suffix-p ".tex" file-name)
                           "doc"
                         "file")))))
               ((symbol-function 'overleaf-project--update-doc-text)
                (lambda (project-id doc-id local-file remote-file)
                  (push `(:update-doc
                          ,project-id
                          ,doc-id
                          ,(file-relative-name local-file)
                          ,(file-relative-name remote-file))
                        events)))
               ((symbol-function 'overleaf-project--update-doc-text-content)
                (lambda (project-id doc-id before after)
                  (push `(:update-metadata-doc
                          ,project-id
                          ,doc-id
                          ,before
                          ,after)
                        events)))
               ((symbol-function 'overleaf-project--warn)
                (lambda (&rest args)
                  (push `(:warn ,@args) events))))
       ,@body)))

(ert-deftest overleaf-project-sync-tree-test-scan-ignores-sync-metadata ()
  (overleaf-project-sync-tree-test--with-temp-dir root
    (let ((overleaf-project-sync-metadata-enabled t)
          (overleaf-project-sync-metadata-file ".overleaf-project-sync.json"))
      (overleaf-project-sync-tree-test--write-file
       root
       ".overleaf-project-sync.json"
       "{}\n")
      (overleaf-project-sync-tree-test--write-file
       root
       "main.tex"
       "body\n")
      (let* ((state (overleaf-project--scan-local-tree root))
             (files (plist-get state :files)))
        (should (gethash "main.tex" files))
        (should-not (gethash ".overleaf-project-sync.json" files))))))

(ert-deftest overleaf-project-sync-tree-test-creates-folders-and-uploads-files ()
  (let ((overleaf-project-sync-metadata-enabled t))
    (overleaf-project-sync-tree-test--with-temp-dir local-root
      (overleaf-project-sync-tree-test--with-temp-dir remote-root
        (overleaf-project-sync-tree-test--write-file
         local-root
         "chapters/intro.tex"
         "intro\n")
        (overleaf-project-sync-tree-test--write-file
         local-root
         "main.tex"
         "main\n")
        (let ((remote-table
               (overleaf-project-sync-tree-test--root-table)))
          (overleaf-project-sync-tree-test--with-remote-stubs
            (overleaf-project--sync-local-tree
             "project-id"
             local-root
             remote-root
             remote-table)
            (let ((events (nreverse events)))
              (should (equal (mapcar #'car events)
                             '(:create-folder :upload :upload)))
              (should (equal (nth 1 (nth 0 events)) "project-id"))
              (should (equal (nth 2 (nth 0 events)) "root"))
              (should (equal (nth 3 (nth 0 events)) "chapters"))
              (should (equal (nth 3 (nth 1 events)) "intro.tex"))
              (should (equal (nth 4 (nth 1 events)) "intro\n"))
              (should (equal (nth 3 (nth 2 events)) "main.tex"))
              (should (equal (nth 4 (nth 2 events)) "main\n")))
            (should (eq (overleaf-project--entity-type
                         (gethash "chapters" remote-table))
                        'folder))
            (should (eq (overleaf-project--entity-type
                         (gethash "chapters/intro.tex" remote-table))
                        'doc))
            (should (eq (overleaf-project--entity-type
                         (gethash "main.tex" remote-table))
                        'doc))))))))

(ert-deftest overleaf-project-sync-tree-test-updates-existing-doc-text ()
  (overleaf-project-sync-tree-test--with-temp-dir local-root
    (overleaf-project-sync-tree-test--with-temp-dir remote-root
      (overleaf-project-sync-tree-test--write-file
       local-root
       "main.tex"
       "new\n")
      (overleaf-project-sync-tree-test--write-file
       remote-root
       "main.tex"
       "old\n")
      (let ((remote-table
             (overleaf-project-sync-tree-test--root-table)))
        (overleaf-project-sync-tree-test--put-entity
         remote-table
         "main.tex"
         "doc-1"
         'doc)
        (overleaf-project-sync-tree-test--with-remote-stubs
          (overleaf-project--sync-local-tree
           "project-id"
           local-root
           remote-root
           remote-table)
          (let ((events (nreverse events)))
            (should (equal (mapcar #'car events) '(:update-doc)))
            (should (equal (nth 2 (car events)) "doc-1"))))))))

(ert-deftest overleaf-project-sync-tree-test-replaces-remote-folder-with-file ()
  (overleaf-project-sync-tree-test--with-temp-dir local-root
    (overleaf-project-sync-tree-test--with-temp-dir remote-root
      (overleaf-project-sync-tree-test--write-file
       local-root
       "asset"
       "file content\n")
      (let ((remote-table
             (overleaf-project-sync-tree-test--root-table)))
        (overleaf-project-sync-tree-test--put-entity
         remote-table
         "asset"
         "folder-1"
         'folder)
        (overleaf-project-sync-tree-test--put-entity
         remote-table
         "asset/old.tex"
         "old-doc"
         'doc
         "folder-1")
        (overleaf-project-sync-tree-test--with-remote-stubs
          (overleaf-project--sync-local-tree
           "project-id"
           local-root
           remote-root
           remote-table)
          (let ((events (nreverse events)))
            (should (equal (mapcar #'car events) '(:delete :upload)))
            (should (equal (nth 2 (nth 0 events)) "asset"))
            (should (equal (nth 3 (nth 1 events)) "asset"))
            (should (equal (nth 4 (nth 1 events)) "file content\n")))
          (should (eq (overleaf-project--entity-type
                       (gethash "asset" remote-table))
                      'file))
          (should-not (gethash "asset/old.tex" remote-table)))))))

(ert-deftest overleaf-project-sync-tree-test-deletes-remote-only-entries-deepest-first ()
  (overleaf-project-sync-tree-test--with-temp-dir local-root
    (overleaf-project-sync-tree-test--with-temp-dir remote-root
      (let ((remote-table
             (overleaf-project-sync-tree-test--root-table)))
        (overleaf-project-sync-tree-test--put-entity
         remote-table
         "old.txt"
         "old-file"
         'file)
        (overleaf-project-sync-tree-test--put-entity
         remote-table
         "dir"
         "dir-folder"
         'folder)
        (overleaf-project-sync-tree-test--put-entity
         remote-table
         "dir/nested.txt"
         "nested-file"
         'doc
         "dir-folder")
        (overleaf-project-sync-tree-test--put-entity
         remote-table
         "dir/sub"
         "sub-folder"
         'folder
         "dir-folder")
        (overleaf-project-sync-tree-test--put-entity
         remote-table
         "dir/sub/deep.txt"
         "deep-file"
         'file
         "sub-folder")
        (overleaf-project-sync-tree-test--with-remote-stubs
          (overleaf-project--sync-local-tree
           "project-id"
           local-root
           remote-root
           remote-table)
          (let ((events (nreverse events)))
            (should (equal (mapcar #'car events)
                           '(:delete :delete :delete :delete :delete)))
            (should (equal (mapcar (lambda (event) (nth 2 event)) events)
                           '("dir/sub/deep.txt"
                             "dir/nested.txt"
                             "old.txt"
                             "dir/sub"
                             "dir")))))
        (should-not (gethash "old.txt" remote-table))
        (should-not (gethash "dir" remote-table))
        (should-not (gethash "dir/nested.txt" remote-table))
        (should-not (gethash "dir/sub" remote-table))
        (should-not (gethash "dir/sub/deep.txt" remote-table))))))

(ert-deftest overleaf-project-sync-tree-test-remote-sync-metadata-is-not-deleted ()
  (let ((overleaf-project-sync-metadata-enabled t)
        (overleaf-project-sync-metadata-file ".overleaf-project-sync.json"))
    (overleaf-project-sync-tree-test--with-temp-dir local-root
      (overleaf-project-sync-tree-test--with-temp-dir remote-root
        (let ((remote-table
               (overleaf-project-sync-tree-test--root-table)))
          (overleaf-project-sync-tree-test--put-entity
           remote-table
           ".overleaf-project-sync.json"
           "metadata-doc"
           'doc)
          (overleaf-project-sync-tree-test--put-entity
           remote-table
           "old.txt"
           "old-file"
           'file)
          (overleaf-project-sync-tree-test--with-remote-stubs
            (overleaf-project--sync-local-tree
             "project-id"
             local-root
             remote-root
             remote-table)
            (let ((events (nreverse events)))
              (should (equal (mapcar (lambda (event) (nth 2 event))
                                     events)
                             '("old.txt")))))
          (should (gethash ".overleaf-project-sync.json" remote-table))
          (should-not (gethash "old.txt" remote-table)))))))

(ert-deftest overleaf-project-sync-tree-test-upload-sync-metadata-new-file ()
  (let ((overleaf-project-sync-metadata-enabled t)
        (overleaf-project-sync-metadata-file ".overleaf-project-sync.json"))
    (let ((remote-table
           (overleaf-project-sync-tree-test--root-table)))
      (overleaf-project-sync-tree-test--with-remote-stubs
        (cl-letf (((symbol-function 'overleaf-project--sync-metadata-json)
                   (lambda (&rest _args) "{\"schema\":1}\n")))
          (overleaf-project--upload-sync-metadata
           "/repo"
           "HEAD"
           "project-id"
           remote-table))
        (let ((events (nreverse events)))
          (should (equal (mapcar #'car events) '(:upload)))
          (should (equal (nth 2 (car events)) "root"))
          (should (equal (nth 3 (car events))
                         ".overleaf-project-sync.json"))
          (should (equal (nth 4 (car events)) "{\"schema\":1}\n")))
        (should (eq (overleaf-project--entity-type
                     (gethash ".overleaf-project-sync.json" remote-table))
                    'file))))))

(ert-deftest overleaf-project-sync-tree-test-upload-sync-metadata-replaces-file ()
  (let ((overleaf-project-sync-metadata-enabled t)
        (overleaf-project-sync-metadata-file ".overleaf-project-sync.json"))
    (let ((remote-table
           (overleaf-project-sync-tree-test--root-table)))
      (overleaf-project-sync-tree-test--put-entity
       remote-table
       ".overleaf-project-sync.json"
       "old-metadata-file"
       'file)
      (overleaf-project-sync-tree-test--with-remote-stubs
        (cl-letf (((symbol-function 'overleaf-project--sync-metadata-json)
                   (lambda (&rest _args) "new metadata\n")))
          (overleaf-project--upload-sync-metadata
           "/repo"
           "HEAD"
           "project-id"
           remote-table))
        (let ((events (nreverse events)))
          (should (equal (mapcar #'car events) '(:delete :upload)))
          (should (equal (nth 2 (nth 0 events))
                         ".overleaf-project-sync.json"))
          (should (equal (nth 4 (nth 1 events)) "new metadata\n")))))))

(ert-deftest overleaf-project-sync-tree-test-upload-sync-metadata-updates-doc ()
  (let ((overleaf-project-sync-metadata-enabled t)
        (overleaf-project-sync-metadata-file ".overleaf-project-sync.json")
        (overleaf-project--remote-sync-metadata-text "old metadata\n"))
    (let ((remote-table
           (overleaf-project-sync-tree-test--root-table)))
      (overleaf-project-sync-tree-test--put-entity
       remote-table
       ".overleaf-project-sync.json"
       "metadata-doc"
       'doc)
      (overleaf-project-sync-tree-test--with-remote-stubs
        (cl-letf (((symbol-function 'overleaf-project--sync-metadata-json)
                   (lambda (&rest _args) "new metadata\n")))
          (overleaf-project--upload-sync-metadata
           "/repo"
           "HEAD"
           "project-id"
           remote-table))
        (let ((events (nreverse events)))
          (should (equal (mapcar #'car events) '(:update-metadata-doc)))
          (should (equal (nth 2 (car events)) "metadata-doc"))
          (should (equal (nth 3 (car events)) "old metadata\n"))
          (should (equal (nth 4 (car events)) "new metadata\n")))
        (should (equal overleaf-project--remote-sync-metadata-text
                       "new metadata\n"))))))

(ert-deftest overleaf-project-sync-tree-test-upload-sync-metadata-warns-without-root-or-text ()
  (let ((overleaf-project-sync-metadata-enabled t)
        (overleaf-project-sync-metadata-file ".overleaf-project-sync.json"))
    (let ((remote-table (make-hash-table :test #'equal)))
      (overleaf-project-sync-tree-test--with-remote-stubs
        (cl-letf (((symbol-function 'overleaf-project--sync-metadata-json)
                   (lambda (&rest _args) "new metadata\n")))
          (overleaf-project--upload-sync-metadata
           "/repo"
           "HEAD"
           "project-id"
           remote-table))
        (should (eq (caar events) :warn))))
    (let ((remote-table
           (overleaf-project-sync-tree-test--root-table))
          (overleaf-project--remote-sync-metadata-text nil))
      (overleaf-project-sync-tree-test--put-entity
       remote-table
       ".overleaf-project-sync.json"
       "metadata-doc"
       'doc)
      (overleaf-project-sync-tree-test--with-remote-stubs
        (cl-letf (((symbol-function 'overleaf-project--sync-metadata-json)
                   (lambda (&rest _args) "new metadata\n")))
          (overleaf-project--upload-sync-metadata
           "/repo"
           "HEAD"
           "project-id"
           remote-table))
        (let ((events (nreverse events)))
          (should (equal (mapcar #'car events) '(:warn))))))))

;;; overleaf-project-sync-tree-test.el ends here
