;;; overleaf-project-http.el --- HTTP and remote tree helpers for overleaf-project -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Overleaf HTTP requests, project discovery, downloads, and remote tree parsing.

;;; Code:

(require 'json)
(require 'mm-url)
(require 'websocket)
(require 'overleaf-project-core)

(declare-function mm-url-decode-entities-string "mm-url")

;;;; HTTP helpers

(defun overleaf-project--format-curl-headers (alist)
  "Format header ALIST into a list of \"Key: Value\" strings for curl."
  (mapcar (lambda (pair) (format "%s: %s" (car pair) (cdr pair))) alist))

(defun overleaf-project--base-headers (&optional referer extra-headers)
  "Return basic authenticated headers with REFERER and EXTRA-HEADERS."
  (append
   `(("Cookie" . ,(overleaf--get-cookies))
     ("Origin" . ,(overleaf--url))
     ("Referer" . ,(or referer (overleaf--url))))
   extra-headers))

(defun overleaf-project--project-headers (&optional project-id extra-headers)
  "Return mutation headers for PROJECT-ID with EXTRA-HEADERS appended."
  (append
   (overleaf-project--base-headers
    (and project-id (overleaf--project-page-url project-id)))
   (when project-id
     `(("Accept" . "application/json")
       ("Cache-Control" . "no-cache")
       ("x-csrf-token" . ,(overleaf-project--csrf-token project-id))))
   extra-headers))

(defun overleaf-project--csrf-cache-key (project-id &optional cookies)
  "Return the csrf cache key for PROJECT-ID and COOKIES."
  (format "%s|%s|%s"
          (overleaf--url)
          project-id
          (secure-hash 'sha1 (or cookies (overleaf--get-cookies)))))

(defun overleaf-project--clear-csrf-cache (&optional project-id)
  "Clear cached csrf token(s), optionally only for PROJECT-ID."
  (if project-id
      (maphash
       (lambda (key _value)
         (when (string-prefix-p (format "%s|%s|" (overleaf--url) project-id) key)
           (remhash key overleaf--csrf-cache)))
       overleaf--csrf-cache)
    (clrhash overleaf--csrf-cache)))

(defun overleaf-project--csrf-token (project-id &optional refresh)
  "Return the csrf token for PROJECT-ID.
If REFRESH is non-nil, bypass the cached token and fetch a fresh one."
  (let* ((cookies (overleaf--get-cookies))
         (cache-key (overleaf-project--csrf-cache-key project-id cookies))
         (cached (gethash cache-key overleaf--csrf-cache)))
    (or (and (not refresh) cached)
        (let* ((project-page
                (overleaf-project--curl-request
                 "GET"
                 (overleaf--project-page-url project-id)
                 (overleaf-project--format-curl-headers
                  `(("Cookie" . ,cookies)
                    ("Origin" . ,(overleaf--url))
                    ("Referer" . ,(overleaf--project-page-url project-id))))))
               (token
                (save-match-data
                  (when (string-match
                         "<meta name=\"ol-csrfToken\" content=\"\\([^\"]+\\)\""
                         project-page)
                    (match-string 1 project-page)))))
          (unless token
            (user-error "Could not extract csrf token for project %s" project-id))
          (puthash cache-key token overleaf--csrf-cache)
          token))))

(defun overleaf-project--curl-403-p (result)
  "Return non-nil if curl RESULT looks like an HTTP 403 failure."
  (and (integerp (overleaf-project--command-result-status result))
       (not (zerop (overleaf-project--command-result-status result)))
       (string-match-p
        "returned error: 403"
        (overleaf-project--command-result-output result))))

(defun overleaf-project--curl-timeout-args ()
  "Return timeout arguments shared by Overleaf curl commands."
  (append
   (when overleaf-curl-connect-timeout
     (list "--connect-timeout"
           (number-to-string overleaf-curl-connect-timeout)))
   (when overleaf-curl-max-time
     (list "--max-time"
           (number-to-string overleaf-curl-max-time)))))

(defun overleaf-project--curl-base-args ()
  "Return common arguments shared by Overleaf curl commands."
  (append
   '("--fail" "--silent" "--show-error" "--location")
   (overleaf-project--curl-timeout-args)))

(defun overleaf-project--socket-cookies ()
  "Return cookies suitable for websocket access."
  (let* ((cookies (overleaf--get-cookies))
         (header-text
          (overleaf-project--curl-header-text
           "GET"
           (format "%s/socket.io/socket.io.js" (overleaf--url))
           (overleaf-project--format-curl-headers
            `(("Cookie" . ,cookies)
              ("Origin" . ,(overleaf--url))))))
         (gclb-cookie
          (and header-text
               (save-match-data
                 (when (string-match "\\(GCLB=.*?\\);" header-text)
                   (match-string 1 header-text))))))
    (if (and gclb-cookie (not (string-empty-p gclb-cookie)))
        (format "%s; %s" cookies gclb-cookie)
      cookies)))

(defun overleaf-project--curl-download-args (url output-file headers)
  "Return curl argument list to download URL into OUTPUT-FILE with HEADERS."
  (append
   (overleaf-project--curl-base-args)
   (apply #'append (mapcar (lambda (h) (list "-H" h)) headers))
   (list "--output" output-file url)))

(defun overleaf-project--curl-download (url output-file headers)
  "Download URL into OUTPUT-FILE with HEADERS using curl."
  (overleaf-project--run overleaf-curl-executable
                         (overleaf-project--curl-download-args
                          url output-file headers)))

(defun overleaf-project--curl-request (method url headers &optional body)
  "Run a curl request with METHOD to URL using HEADERS and optional BODY."
  (let ((args
         (append
          (overleaf-project--curl-base-args)
          (list "-X" method)
          (apply
           #'append
           (mapcar (lambda (header) (list "-H" header)) headers))
          (when body
            (list "--data-binary" body))
          (list url))))
    (overleaf-project--command-result-output
     (overleaf-project--run overleaf-curl-executable args))))

(defun overleaf-project--curl-header-text (method url headers &optional body)
  "Run a curl request and return raw response headers as text.
METHOD, URL, HEADERS, and optional BODY are passed through to curl.
The response body is discarded."
  (let ((args
         (append
          (overleaf-project--curl-base-args)
          (list "-X" method "--dump-header" "-" "--output" null-device)
          (apply
           #'append
           (mapcar (lambda (header) (list "-H" header)) headers))
          (when body
            (list "--data-binary" body))
          (list url))))
    (overleaf-project--command-result-output
     (overleaf-project--run overleaf-curl-executable args))))

(defun overleaf-project--curl-upload-file
    (project-id folder-id file-name file-path)
  "Upload FILE-PATH as FILE-NAME into FOLDER-ID on PROJECT-ID."
  (cl-labels
      ((build-args ()
         (let* ((url
                 (format "%s/project/%s/upload?folder_id=%s"
                         (overleaf--url)
                         project-id
                         folder-id))
                (headers
                 (overleaf-project--format-curl-headers
                  (overleaf-project--project-headers project-id))))
           (append
            (overleaf-project--curl-base-args)
            (list "-X" "POST")
            (apply #'append
                   (mapcar (lambda (header) (list "-H" header)) headers))
            (list
             "-F" "relativePath=null"
             "-F" (format "name=%s" file-name)
             "-F" "type=application/octet-stream"
             "-F"
             (format "qqfile=@%s;type=application/octet-stream" file-path)
             url)))))
    (let* ((args (build-args))
           (result
            (overleaf-project--run overleaf-curl-executable args nil nil t)))
      (when (overleaf-project--curl-403-p result)
        (overleaf--warn
         "Overleaf upload returned 403; refreshing csrf token and retrying once")
        (overleaf-project--clear-csrf-cache project-id)
        (setq args (build-args)
              result
              (overleaf-project--run overleaf-curl-executable args nil nil t)))
      (unless (and (integerp (overleaf-project--command-result-status result))
                   (zerop (overleaf-project--command-result-status result)))
        (error "%s %s failed: %s"
               (overleaf-project--ensure-executable overleaf-curl-executable)
               (string-join args " ")
               (let ((output (overleaf-project--command-result-output result)))
                 (if (string-empty-p output) "unknown error" output))))
      (json-parse-string
       (overleaf-project--command-result-output result)
       :object-type 'plist
       :array-type 'list))))

(defun overleaf-project--download-snapshot (project-id)
  "Download PROJECT-ID as a temporary snapshot."
  (let* ((zipfile (make-temp-file "overleaf-project." nil ".zip"))
         (temp-dir (make-temp-file "overleaf-project." t))
         (headers
          (overleaf-project--format-curl-headers
           (overleaf-project--base-headers
            (overleaf--project-page-url project-id))))
         (url
          (format "%s/project/%s/download/zip"
                  (overleaf--url)
                  project-id)))
    (let ((success nil))
      (unwind-protect
          (prog1
              (progn
                (overleaf--message "Downloading project %s..." project-id)
                (overleaf-project--curl-download url zipfile headers)
                (overleaf-project--run
                 overleaf-unzip-executable
                 (list "-q" "-o" zipfile "-d" temp-dir))
                (make-overleaf-project--snapshot
                 :temp-dir temp-dir
                 :root (overleaf-project--normalize-extracted-root temp-dir)))
            (setq success t))
        (ignore-errors (delete-file zipfile))
        (unless success
          (ignore-errors (delete-directory temp-dir t)))))))

(defun overleaf-project--fetch-remote-table (project-id)
  "Return the remote entity table for PROJECT-ID."
  (overleaf-project--build-entity-table
   (overleaf-project--fetch-tree project-id)))

(defun overleaf-project--create-folder (project-id parent-id name)
  "Create folder NAME below PARENT-ID on PROJECT-ID."
  (json-parse-string
   (overleaf-project--curl-request
    "POST"
    (format "%s/project/%s/folder" (overleaf--url) project-id)
    (overleaf-project--format-curl-headers
     (append
      (overleaf-project--project-headers project-id)
      '(("Content-Type" . "application/json"))))
    (json-encode `(:parent_folder_id ,parent-id :name ,name)))
   :object-type 'plist
   :array-type 'list))

(defun overleaf-project--delete-entity (project-id entity)
  "Delete ENTITY from PROJECT-ID."
  (let ((entity-type
         (pcase (overleaf-project--entity-type entity)
           ('folder "folder")
           ('doc "doc")
           ('file "file")
           (_ (user-error "Unsupported entity type: %S"
                          (overleaf-project--entity-type entity))))))
    (overleaf-project--curl-request
     "DELETE"
     (format "%s/project/%s/%s/%s"
             (overleaf--url)
             project-id
             entity-type
             (overleaf-project--entity-id entity))
     (overleaf-project--format-curl-headers
      (append
       (overleaf-project--project-headers project-id)
       '(("Content-Type" . "application/json"))))
     "{}")))

;;;; Project discovery

(defun overleaf-project-list (&optional url)
  "Return the list of accessible Overleaf projects for URL."
  (setq overleaf-url (or url (overleaf--url)))
  (let* ((cookies (overleaf--get-cookies))
         (project-page
          (overleaf-project--curl-request
           "GET"
           (format "%s/project" (overleaf--url))
           (overleaf-project--format-curl-headers
            `(("Cookie" . ,cookies)
              ("Origin" . ,(overleaf--url))))))
         (projects-json
          (save-match-data
            (unless (string-match
                     "name=\"ol-prefetchedProjectsBlob\".*?content=\"\\(.*?\\)\""
                     project-page)
              (user-error "Could not find project list on %s" (overleaf--url)))
            (json-parse-string
             (url-unhex-string
              (mm-url-decode-entities-string (match-string 1 project-page)))
             :object-type 'plist
             :array-type 'list))))
    (plist-get projects-json :projects)))

(defun overleaf-project--select-project (projects)
  "Prompt for one project from PROJECTS and return its plist."
  (unless projects
    (user-error "No accessible Overleaf projects were found"))
  (let ((collection
         (mapcar
          (lambda (project)
            `(:fields
              (,(plist-get project :name)
               ,(or (plist-get (plist-get project :owner) :email) ""))
              :data ,project))
          projects)))
    (overleaf--completing-read "Project: " collection)))

(defun overleaf-project--read-project (&optional url)
  "Prompt for an Overleaf project on URL and return its plist."
  (overleaf-project--select-project (overleaf-project-list url)))

;;;; Remote project tree

(defun overleaf-project--fetch-tree (project-id)
  "Return PROJECT-ID's root folder plist via websocket."
  (let* ((cookies (overleaf-project--socket-cookies))
         (response-body
          (overleaf-project--curl-request
           "GET"
           (format "%s/socket.io/1/?projectId=%s&esh=1&ssp=1"
                   (overleaf--url)
                   project-id)
           (overleaf-project--format-curl-headers
            `(("Cookie" . ,cookies)
              ("Origin" . ,(overleaf--url))))))
         (ws-id (car (string-split response-body ":")))
         (ws-url
          (replace-regexp-in-string
           "^http" "ws"
           (replace-regexp-in-string
            "^https" "wss"
            (format "%s/socket.io/1/websocket/%s?projectId=%s&esh=1&ssp=1"
                    (overleaf--url)
                    ws-id
                    project-id))))
         (done nil)
         (result nil)
         (failure nil)
         (ws nil))
    (unwind-protect
        (progn
          (setq ws
                (websocket-open
                 ws-url
                 :custom-header-alist `(("Cookie" . ,cookies)
                                        ("Origin" . ,(overleaf--url)))
                 :on-message
                 (lambda (socket frame)
                   (let ((text (websocket-frame-text frame)))
                     (overleaf--debug "Websocket frame: %s" text)
                     (cond
                      ((string-prefix-p "2::" text)
                       (websocket-send-text socket "2::"))
                      ((string-prefix-p "7:" text)
                       (setq failure "Unauthorized websocket response"
                             done t)
                       (ignore-errors (websocket-close socket)))
                      ((string-prefix-p "5:" text)
                       (condition-case err
                           (let* ((payload
                                   (overleaf-project--socketio-event-data text))
                                  (message
                                   (json-parse-string
                                    payload
                                    :object-type 'plist
                                    :array-type 'list)))
                             (when (string=
                                    (plist-get message :name)
                                    "joinProjectResponse")
                               (setq result
                                     (overleaf--pget
                                      message :args 0 :project :rootFolder 0)
                                     done t)
                               (ignore-errors (websocket-close socket))))
                         (error
                          (setq failure (error-message-string err)
                                done t)
                          (ignore-errors (websocket-close socket))))))))
                 :on-close
                 (lambda (_socket)
                   (setq done t))))
          (let ((deadline (+ (float-time) overleaf-project-socket-timeout)))
            (while (and (not done) (< (float-time) deadline))
              (accept-process-output nil 0.1))
            (unless done
              (setq failure "Timed out while waiting for project tree"))
            (when failure
              (user-error "Could not fetch project tree: %s" failure))
            (or result
                (user-error "Could not fetch project tree for %s" project-id))))
      (when ws
        (ignore-errors (websocket-close ws))))))

(defun overleaf-project--socketio-event-data (text)
  "Return the JSON payload from a Socket.IO 0.9 event frame TEXT."
  (save-match-data
    (unless (string-match "\\`5:[^:]*:[^:]*:\\(.*\\)\\'" text)
      (error "Unsupported websocket event frame: %s" text))
    (match-string 1 text)))

(defun overleaf-project--build-entity-table (root-folder)
  "Return a hash table containing all remote entities from ROOT-FOLDER."
  (let ((table (make-hash-table :test #'equal)))
    (cl-labels
        ((walk-folder (folder parent-path parent-id)
           (let* ((name (plist-get folder :name))
                  (id (plist-get folder :_id))
                  (path (if (string= name "rootFolder")
                            ""
                          (if (string-empty-p parent-path)
                              name
                            (concat parent-path "/" name)))))
             (puthash
              path
              (make-overleaf-project--entity
               :path path
               :name name
               :id id
               :type 'folder
               :parent-id parent-id)
              table)
             (dolist (doc (plist-get folder :docs))
               (let ((doc-path
                      (if (string-empty-p path)
                          (plist-get doc :name)
                        (concat path "/" (plist-get doc :name)))))
                 (puthash
                  doc-path
                  (make-overleaf-project--entity
                   :path doc-path
                   :name (plist-get doc :name)
                   :id (plist-get doc :_id)
                   :type 'doc
                   :parent-id id)
                  table)))
             (dolist (file (plist-get folder :fileRefs))
               (let ((file-path
                      (if (string-empty-p path)
                          (plist-get file :name)
                        (concat path "/" (plist-get file :name)))))
                 (puthash
                  file-path
                  (make-overleaf-project--entity
                   :path file-path
                   :name (plist-get file :name)
                   :id (plist-get file :_id)
                   :type 'file
                   :parent-id id)
                  table)))
             (dolist (child (plist-get folder :folders))
               (walk-folder child path id)))))
      (walk-folder root-folder "" nil))
    table))

(defun overleaf-project--forget-entry (table path)
  "Delete PATH and all descendants from TABLE."
  (let ((prefix (if (string-empty-p path) path (concat path "/")))
        keys)
    (maphash
     (lambda (key _value)
       (when (or (string= key path)
                 (and (not (string-empty-p prefix))
                      (string-prefix-p prefix key)))
         (push key keys)))
     table)
    (dolist (key keys)
      (remhash key table))))


(provide 'overleaf-project-http)

;;; overleaf-project-http.el ends here
