;;; overleaf-project-http.el --- HTTP and remote tree helpers for overleaf-project -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 Jamie Cui
;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Assisted-by: Codex:GPT-5.5
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Overleaf HTTP requests, project discovery, downloads, remote tree
;; parsing, and real-time document updates.

;;; Code:

(require 'json)
(require 'mm-url)
(require 'websocket)
(require 'overleaf-project-core)

(declare-function overleaf-project--async-register-process "overleaf-project-core")
(declare-function overleaf-project--async-unregister-process "overleaf-project-core")
(declare-function mm-url-decode-entities-string "mm-url")

(cl-defstruct overleaf-project--socketio-client
  "Minimal Socket.IO 0.9 client state."
  websocket
  done
  failure
  next-ack-id
  acks
  events)

;;;; HTTP helpers

(defun overleaf-project--format-curl-headers (alist)
  "Format header ALIST into a list of \"Key: Value\" strings for curl."
  (mapcar (lambda (pair) (format "%s: %s" (car pair) (cdr pair))) alist))

(defun overleaf-project--base-headers (&optional referer extra-headers)
  "Return basic authenticated headers with REFERER and EXTRA-HEADERS."
  (append
   `(("Cookie" . ,(overleaf-project--get-cookies))
     ("Origin" . ,(overleaf-project--url))
     ("Referer" . ,(or referer (overleaf-project--url))))
   extra-headers))

(defun overleaf-project--project-headers (&optional project-id extra-headers)
  "Return mutation headers for PROJECT-ID with EXTRA-HEADERS appended."
  (append
   (overleaf-project--base-headers
    (and project-id (overleaf-project--project-page-url project-id)))
   (when project-id
     `(("Accept" . "application/json")
       ("Cache-Control" . "no-cache")
       ("x-csrf-token" . ,(overleaf-project--csrf-token project-id))))
   extra-headers))

(defun overleaf-project--csrf-cache-key (project-id &optional cookies)
  "Return the csrf cache key for PROJECT-ID and COOKIES."
  (format "%s|%s|%s"
          (overleaf-project--url)
          project-id
          (secure-hash 'sha1 (or cookies (overleaf-project--get-cookies)))))

(defun overleaf-project--clear-csrf-cache (&optional project-id)
  "Clear cached csrf token(s), optionally only for PROJECT-ID."
  (if project-id
      (maphash
       (lambda (key _value)
         (when (string-prefix-p (format "%s|%s|" (overleaf-project--url) project-id) key)
           (remhash key overleaf-project--csrf-cache)))
       overleaf-project--csrf-cache)
    (clrhash overleaf-project--csrf-cache)))

(defun overleaf-project--csrf-token (project-id &optional refresh)
  "Return the csrf token for PROJECT-ID.
If REFRESH is non-nil, bypass the cached token and fetch a fresh one."
  (let* ((cookies (overleaf-project--get-cookies))
         (cache-key (overleaf-project--csrf-cache-key project-id cookies))
         (cached (gethash cache-key overleaf-project--csrf-cache)))
    (or (and (not refresh) cached)
        (let* ((project-page
                (overleaf-project--curl-request
                 "GET"
                 (overleaf-project--project-page-url project-id)
                 (overleaf-project--format-curl-headers
                  `(("Cookie" . ,cookies)
                    ("Origin" . ,(overleaf-project--url))
                    ("Referer" . ,(overleaf-project--project-page-url project-id))))))
               (token
                (save-match-data
                  (when (string-match
                         "<meta name=\"ol-csrfToken\" content=\"\\([^\"]+\\)\""
                         project-page)
                    (match-string 1 project-page)))))
          (unless token
            (user-error "Could not extract csrf token for project %s" project-id))
          (puthash cache-key token overleaf-project--csrf-cache)
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
   (when overleaf-project--curl-connect-timeout
     (list "--connect-timeout"
           (number-to-string overleaf-project--curl-connect-timeout)))
   (when overleaf-project--curl-max-time
     (list "--max-time"
           (number-to-string overleaf-project--curl-max-time)))))

(defun overleaf-project--curl-download-timeout-args ()
  "Return timeout arguments for project zip downloads."
  (append
   (when overleaf-project--curl-connect-timeout
     (list "--connect-timeout"
           (number-to-string overleaf-project--curl-connect-timeout)))
   (when overleaf-project--curl-download-max-time
     (list "--max-time"
           (number-to-string overleaf-project--curl-download-max-time)))
   (when (and overleaf-project--curl-download-speed-limit
              overleaf-project--curl-download-speed-time)
     (list "--speed-limit"
           (number-to-string overleaf-project--curl-download-speed-limit)
           "--speed-time"
           (number-to-string overleaf-project--curl-download-speed-time)))))

(defun overleaf-project--curl-base-args ()
  "Return common arguments shared by Overleaf curl commands."
  (append
   '("--fail" "--silent" "--show-error" "--location")
   (overleaf-project--curl-timeout-args)))

(defun overleaf-project--curl-download-base-args ()
  "Return common arguments shared by Overleaf project zip downloads."
  (append
   '("--fail" "--silent" "--show-error" "--location")
   (overleaf-project--curl-download-timeout-args)))

(defun overleaf-project--socket-cookies ()
  "Return cookies suitable for websocket access."
  (let* ((cookies (overleaf-project--get-cookies))
         (header-text
          (overleaf-project--curl-header-text
           "GET"
           (format "%s/socket.io/socket.io.js" (overleaf-project--url))
           (overleaf-project--format-curl-headers
            `(("Cookie" . ,cookies)
              ("Origin" . ,(overleaf-project--url))))))
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
   (overleaf-project--curl-download-base-args)
   (apply #'append (mapcar (lambda (h) (list "-H" h)) headers))
   (list "--output" output-file url)))

(defun overleaf-project--curl-download (url output-file headers)
  "Download URL into OUTPUT-FILE with HEADERS using curl."
  (overleaf-project--run overleaf-project-curl-executable
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
     (overleaf-project--run overleaf-project-curl-executable args))))

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
     (overleaf-project--run overleaf-project-curl-executable args))))

(defun overleaf-project--curl-upload-file
    (project-id folder-id file-name file-path)
  "Upload FILE-PATH as FILE-NAME into FOLDER-ID on PROJECT-ID."
  (cl-labels
      ((build-args ()
         (let* ((url
                 (format "%s/project/%s/upload?folder_id=%s"
                         (overleaf-project--url)
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
            (overleaf-project--run overleaf-project-curl-executable args nil nil t)))
      (when (overleaf-project--curl-403-p result)
        (overleaf-project--warn
         "Overleaf upload returned 403; refreshing csrf token and retrying once")
        (overleaf-project--clear-csrf-cache project-id)
        (setq args (build-args)
              result
              (overleaf-project--run overleaf-project-curl-executable args nil nil t)))
      (unless (and (integerp (overleaf-project--command-result-status result))
                   (zerop (overleaf-project--command-result-status result)))
        (error "%s"
               (overleaf-project--command-error-message
                (overleaf-project--ensure-executable overleaf-project-curl-executable)
                args
                (overleaf-project--command-result-output result))))
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
            (overleaf-project--project-page-url project-id))))
         (url
          (format "%s/project/%s/download/zip"
                  (overleaf-project--url)
                  project-id)))
    (let ((success nil))
      (unwind-protect
          (prog1
              (progn
                (overleaf-project--message "Downloading project %s..." project-id)
                (overleaf-project--curl-download url zipfile headers)
                (overleaf-project--run
                 overleaf-project-unzip-executable
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
    (format "%s/project/%s/folder" (overleaf-project--url) project-id)
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
             (overleaf-project--url)
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
  (setq overleaf-project-url (or url (overleaf-project--url)))
  (let* ((cookies (overleaf-project--get-cookies))
         (project-page
          (overleaf-project--curl-request
           "GET"
           (format "%s/project" (overleaf-project--url))
           (overleaf-project--format-curl-headers
            `(("Cookie" . ,cookies)
              ("Origin" . ,(overleaf-project--url))))))
         (projects-json
          (save-match-data
            (unless (string-match
                     "name=\"ol-prefetchedProjectsBlob\".*?content=\"\\(.*?\\)\""
                     project-page)
              (user-error "Could not find project list on %s" (overleaf-project--url)))
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
    (overleaf-project--completing-read "Project: " collection)))

(defun overleaf-project--read-project (&optional url)
  "Prompt for an Overleaf project on URL and return its plist."
  (overleaf-project--select-project (overleaf-project-list url)))

;;;; Remote project tree

(defun overleaf-project--socketio-connect (project-id)
  "Open a Socket.IO 0.9 websocket for PROJECT-ID."
  (let* ((cookies (overleaf-project--socket-cookies))
         (response-body
          (overleaf-project--curl-request
           "GET"
           (format "%s/socket.io/1/?projectId=%s&esh=1&ssp=1"
                   (overleaf-project--url)
                   project-id)
           (overleaf-project--format-curl-headers
            `(("Cookie" . ,cookies)
              ("Origin" . ,(overleaf-project--url))))))
         (ws-id (car (string-split response-body ":")))
         (ws-url
          (replace-regexp-in-string
           "^http" "ws"
           (replace-regexp-in-string
            "^https" "wss"
            (format "%s/socket.io/1/websocket/%s?projectId=%s&esh=1&ssp=1"
                    (overleaf-project--url)
                    ws-id
                    project-id))))
         (client (make-overleaf-project--socketio-client
                  :next-ack-id 0
                  :acks nil
                  :events nil)))
    (setf
     (overleaf-project--socketio-client-websocket client)
     (websocket-open
      ws-url
      :custom-header-alist `(("Cookie" . ,cookies)
                             ("Origin" . ,(overleaf-project--url)))
      :on-message
      (lambda (socket frame)
        (overleaf-project--socketio-handle-frame
         client
         socket
         (websocket-frame-text frame)))
      :on-close
      (lambda (_socket)
        (setf (overleaf-project--socketio-client-done client) t))))
    (overleaf-project--async-register-process
     (websocket-conn (overleaf-project--socketio-client-websocket client)))
    client))

(defun overleaf-project--socketio-close (client)
  "Close Socket.IO CLIENT."
  (when-let* ((ws (overleaf-project--socketio-client-websocket client)))
    (overleaf-project--async-unregister-process (websocket-conn ws))
    (ignore-errors (websocket-close ws))))

(defun overleaf-project--socketio-handle-frame (client socket text)
  "Handle Socket.IO frame TEXT for CLIENT received from SOCKET."
  (overleaf-project--debug "Websocket frame: %s" text)
  (cond
   ((string-prefix-p "2::" text)
    (websocket-send-text socket "2::"))
   ((string-prefix-p "7:" text)
    (setf (overleaf-project--socketio-client-failure client)
          "Unauthorized websocket response")
    (setf (overleaf-project--socketio-client-done client) t)
    (ignore-errors (websocket-close socket)))
   ((string-prefix-p "5:" text)
    (condition-case err
        (overleaf-project--socketio-handle-event client text)
      (error
       (setf (overleaf-project--socketio-client-failure client)
             (error-message-string err))
       (setf (overleaf-project--socketio-client-done client) t)
       (ignore-errors (websocket-close socket)))))
   ((string-prefix-p "6:" text)
    (condition-case err
        (overleaf-project--socketio-handle-ack client text)
      (error
       (setf (overleaf-project--socketio-client-failure client)
             (error-message-string err))
       (setf (overleaf-project--socketio-client-done client) t)
       (ignore-errors (websocket-close socket)))))))

(defun overleaf-project--socketio-handle-event (client text)
  "Handle Socket.IO event frame TEXT for CLIENT."
  (let* ((payload (overleaf-project--socketio-event-data text))
         (message (json-parse-string
                   payload
                   :object-type 'plist
                   :array-type 'list
                   :null-object nil
                   :false-object nil))
         (name (plist-get message :name)))
    (push message (overleaf-project--socketio-client-events client))
    (when (string= name "connectionRejected")
      (setf (overleaf-project--socketio-client-failure client)
            (format "Overleaf rejected websocket connection: %S"
                    (plist-get message :args)))
      (setf (overleaf-project--socketio-client-done client) t))))

(defun overleaf-project--socketio-handle-ack (client text)
  "Handle Socket.IO ack frame TEXT for CLIENT."
  (save-match-data
    (unless (string-match "\\`6:[^:]*:[^:]*:\\([0-9]+\\)\\(?:+\\(.*\\)\\)?\\'" text)
      (error "Unsupported websocket ack frame: %s" text))
    (let* ((ack-id (match-string 1 text))
           (payload (match-string 2 text))
           (args (if (and payload (not (string-empty-p payload)))
                     (json-parse-string
                      payload
                      :object-type 'plist
                      :array-type 'list
                      :null-object nil
                      :false-object nil)
                   nil)))
      (push (cons ack-id args)
            (overleaf-project--socketio-client-acks client)))))

(defun overleaf-project--socketio-event-data (text)
  "Return the JSON payload from a Socket.IO 0.9 event frame TEXT."
  (save-match-data
    (unless (string-match "\\`5:[^:]*:[^:]*:\\(.*\\)\\'" text)
      (error "Unsupported websocket event frame: %s" text))
    (match-string 1 text)))

(defun overleaf-project--socketio-take-event-if (client predicate)
  "Return and remove the oldest queued CLIENT event matching PREDICATE."
  (let ((events (nreverse (overleaf-project--socketio-client-events client)))
        (matched nil)
        (remaining nil))
    (dolist (event events)
      (if (and (not matched)
               (funcall predicate event))
          (setq matched event)
        (push event remaining)))
    (setf (overleaf-project--socketio-client-events client)
          remaining)
    matched))

(defun overleaf-project--socketio-take-event (client name)
  "Return and remove the oldest queued CLIENT event named NAME."
  (overleaf-project--socketio-take-event-if
   client
   (lambda (event)
     (string= (plist-get event :name) name))))

(defconst overleaf-project--socketio-wait-pending
  (make-symbol "overleaf-project-socketio-wait-pending")
  "Sentinel returned by Socket.IO wait predicates that are not ready.")

(defun overleaf-project--socketio-wait (client predicate description)
  "Wait until PREDICATE returns a non-sentinel value for CLIENT.
Signal an error labelled with DESCRIPTION on timeout or websocket
failure.  PREDICATE must return
`overleaf-project--socketio-wait-pending' while it is still waiting."
  (let ((deadline (+ (float-time) overleaf-project-socket-timeout))
        (result overleaf-project--socketio-wait-pending))
    (while (and (eq result overleaf-project--socketio-wait-pending)
                (not (overleaf-project--socketio-client-done client))
                (< (float-time) deadline))
      (setq result (funcall predicate))
      (when (eq result overleaf-project--socketio-wait-pending)
        (accept-process-output nil 0.1)))
    (when (eq result overleaf-project--socketio-wait-pending)
      (setq result (funcall predicate)))
    (when (eq result overleaf-project--socketio-wait-pending)
      (let ((failure (or (overleaf-project--socketio-client-failure client)
                         (and (>= (float-time) deadline)
                              (format "Timed out while waiting for %s"
                                      description))
                         (format "Websocket closed while waiting for %s"
                                 description))))
        (user-error "%s" failure)))
    result))

(defun overleaf-project--socketio-wait-event (client name)
  "Wait for a Socket.IO event named NAME from CLIENT."
  (overleaf-project--socketio-wait
   client
   (lambda ()
     (or (overleaf-project--socketio-take-event client name)
         overleaf-project--socketio-wait-pending))
   (format "websocket event `%s'" name)))

(defun overleaf-project--socketio-emit (client name &rest args)
  "Emit Socket.IO event NAME with ARGS on CLIENT and wait for its ack.
Return the ack arguments as a list.  Socket.IO acks with no arguments
are returned as nil."
  (let* ((ack-id
          (number-to-string
           (cl-incf
            (overleaf-project--socketio-client-next-ack-id client))))
         (payload (json-encode (list :name name :args args)))
         (frame (format "5:%s+::%s" ack-id payload))
         (ws (overleaf-project--socketio-client-websocket client)))
    (overleaf-project--debug "Websocket send: %s" frame)
    (websocket-send-text ws frame)
    (let ((ack
           (overleaf-project--socketio-wait
            client
            (lambda ()
              (if-let* ((entry
                         (assoc
                          ack-id
                          (overleaf-project--socketio-client-acks client))))
                  (cdr entry)
                overleaf-project--socketio-wait-pending))
            (format "websocket ack for `%s'" name))))
      (setf (overleaf-project--socketio-client-acks client)
            (assoc-delete-all
             ack-id
             (overleaf-project--socketio-client-acks client)))
      ack)))

(defun overleaf-project--socketio-call (project-id function)
  "Call FUNCTION with a connected Socket.IO client for PROJECT-ID.
FUNCTION receives the client and the initial `joinProjectResponse'
event."
  (let ((client nil)
        (join-event nil))
    (unwind-protect
        (progn
          (setq client (overleaf-project--socketio-connect project-id))
          (setq join-event
                (overleaf-project--socketio-wait-event
                 client
                 "joinProjectResponse"))
          (funcall function client join-event))
      (when client
        (overleaf-project--socketio-close client)))))

(defun overleaf-project--fetch-tree (project-id)
  "Return PROJECT-ID's root folder plist via websocket."
  (overleaf-project--socketio-call
   project-id
   (lambda (_client join-event)
     (or (overleaf-project--pget
          join-event
          :args 0 :project :rootFolder 0)
         (user-error "Could not fetch project tree for %s" project-id)))))

;;;; Remote document updates

(defun overleaf-project--file-string (file)
  "Return FILE contents as a raw Emacs string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents-literally file))
    (buffer-string)))

(defun overleaf-project--file-utf8-string (file)
  "Return FILE contents decoded as UTF-8 text."
  (let ((string (decode-coding-string
                 (overleaf-project--file-string file)
                 'utf-8-unix)))
    (when (cl-some
           (lambda (char)
             (eq (char-charset char) 'eight-bit))
           string)
      (user-error
       "Overleaf doc text update requires valid UTF-8 text: %s"
       file))
    string))

(defun overleaf-project--decode-socketio-doc-line (line doc-id)
  "Decode one LINE returned by Overleaf's Socket.IO `joinDoc' for DOC-ID."
  (when (cl-some (lambda (char) (> char #xff)) line)
    (user-error "Could not decode Overleaf doc %s text from websocket" doc-id))
  (let* ((bytes (make-string (length line) 0))
         (string nil))
    (dotimes (index (length line))
      (aset bytes index (aref line index)))
    (setq string (decode-coding-string bytes 'utf-8-unix))
    (when (cl-some
           (lambda (char)
             (eq (char-charset char) 'eight-bit))
           string)
      (user-error "Could not decode Overleaf doc %s text as UTF-8" doc-id))
    string))

(defun overleaf-project--utf-16-length (string)
  "Return STRING length in JavaScript UTF-16 code units."
  (/ (string-bytes (encode-coding-string string 'utf-16le)) 2))

(defun overleaf-project--sharejs-text-op (before after)
  "Return a ShareJS text operation changing BEFORE into AFTER."
  (unless (string= before after)
    (let* ((before-length (length before))
           (after-length (length after))
           (prefix 0)
           (suffix 0))
      (while (and (< prefix before-length)
                  (< prefix after-length)
                  (eq (aref before prefix) (aref after prefix)))
        (setq prefix (1+ prefix)))
      (while (and (< suffix (- before-length prefix))
                  (< suffix (- after-length prefix))
                  (eq (aref before (- before-length suffix 1))
                      (aref after (- after-length suffix 1))))
        (setq suffix (1+ suffix)))
      (let ((deleted (substring before prefix (- before-length suffix)))
            (inserted (substring after prefix (- after-length suffix)))
            (position (overleaf-project--utf-16-length
                       (substring before 0 prefix)))
            (ops nil))
        (unless (string-empty-p deleted)
          (push `(:d ,deleted :p ,position) ops))
        (unless (string-empty-p inserted)
          (push `(:i ,inserted :p ,position) ops))
        (vconcat (nreverse ops))))))

(defun overleaf-project--remote-doc-state (client doc-id)
  "Join DOC-ID via CLIENT and return its current text state."
  (let* ((ack (overleaf-project--socketio-emit
               client
               "joinDoc"
               doc-id
               -1
               '(:encodeRanges t :supportsHistoryOT t)))
         (error-object (car ack))
         (lines (nth 1 ack))
         (version (nth 2 ack))
         (type (nth 5 ack)))
    (when error-object
      (user-error "Could not join Overleaf doc %s: %s" doc-id error-object))
    (unless (equal type "sharejs-text-ot")
      (user-error
       "Overleaf doc %s uses unsupported OT type `%s'; only sharejs-text-ot is supported"
       doc-id
       type))
    (unless (integerp version)
      (user-error "Could not determine Overleaf doc version for %s" doc-id))
    (unless (and (listp lines)
                 (cl-every #'stringp lines))
      (user-error "Could not read Overleaf doc text for %s" doc-id))
    `(:version ,version
               :text ,(string-join
                       (mapcar
                        (lambda (line)
                          (overleaf-project--decode-socketio-doc-line line doc-id))
                        lines)
                       "\n"))))

(defun overleaf-project--socketio-error-message (object)
  "Return a concise message for Socket.IO error OBJECT."
  (cond
   ((null object) nil)
   ((stringp object) object)
   ((and (listp object)
         (plist-get object :message))
    (plist-get object :message))
   (t (format "%S" object))))

(defun overleaf-project--doc-update-event-doc-id (event)
  "Return the document id associated with Overleaf update EVENT."
  (let ((args (plist-get event :args)))
    (pcase (plist-get event :name)
      ("otUpdateApplied"
       (plist-get (car args) :doc))
      ("otUpdateError"
       (plist-get (cadr args) :doc_id)))))

(defun overleaf-project--source-doc-update-applied-p (event doc-id)
  "Return non-nil if EVENT is this client's applied update for DOC-ID."
  (and
   (string= (plist-get event :name) "otUpdateApplied")
   (let ((update (car (plist-get event :args))))
     (and (equal (plist-get update :doc) doc-id)
          (not (plist-member update :op))))))

(defun overleaf-project--wait-doc-update-applied (client doc-id)
  "Wait on CLIENT until DOC-ID's queued OT update is applied or rejected."
  (let ((event
         (overleaf-project--socketio-wait
          client
          (lambda ()
            (or
             (overleaf-project--socketio-take-event-if
              client
              (lambda (event)
                (or
                 (overleaf-project--source-doc-update-applied-p event doc-id)
                 (and
                  (string= (plist-get event :name) "otUpdateError")
                  (equal
                   (overleaf-project--doc-update-event-doc-id event)
                   doc-id)))))
             overleaf-project--socketio-wait-pending))
          (format "Overleaf doc `%s' update to be applied" doc-id))))
    (pcase (plist-get event :name)
      ("otUpdateError"
       (let* ((args (plist-get event :args))
              (error-object (car args))
              (message (cadr args))
              (detail
               (or (overleaf-project--socketio-error-message error-object)
                   (and (listp message)
                        (overleaf-project--socketio-error-message
                         (plist-get message :error)))
                   "document updater rejected the update")))
         (user-error
          "Could not update Overleaf doc %s through text OT: %s"
          doc-id
          detail)))
      ("otUpdateApplied" t)
      (_ t))))

(defun overleaf-project--update-doc-text-content
    (project-id doc-id before after)
  "Update existing DOC-ID on PROJECT-ID from text BEFORE to AFTER.
The update is sent through Overleaf's real-time ShareJS text OT path, so
the remote document id and Overleaf edit history are preserved."
  (let ((op (overleaf-project--sharejs-text-op before after)))
    (when op
      (overleaf-project--socketio-call
       project-id
       (lambda (client _join-event)
         (let* ((state (overleaf-project--remote-doc-state client doc-id))
                (version (plist-get state :version))
                (remote-text (plist-get state :text))
                (ack nil))
           (unless (string= before remote-text)
             (user-error
              "Remote Overleaf doc %s changed after the snapshot was downloaded; run `overleaf-project-pull' and retry"
              doc-id))
           (setq ack
                 (overleaf-project--socketio-emit
                  client
                  "applyOtUpdate"
                  doc-id
                  `(:doc ,doc-id
                         :op ,op
                         :v ,version
                         :meta (:source "overleaf-project"))))
           (when-let* ((error-object (car ack)))
             (user-error
              "Could not update Overleaf doc %s through text OT: %s"
              doc-id
              (or (overleaf-project--socketio-error-message error-object)
                  error-object)))
           (overleaf-project--wait-doc-update-applied client doc-id)
           (unless (string= after
                            (plist-get
                             (overleaf-project--remote-doc-state client doc-id)
                             :text))
             (user-error
              "Overleaf doc %s did not match the expected text after OT update; run `overleaf-project-pull' and retry"
              doc-id))
           (overleaf-project--message
            "Updated Overleaf doc %s through text OT"
            doc-id)
           t))))))

(defun overleaf-project--update-doc-text
    (project-id doc-id local-file remote-file)
  "Update existing DOC-ID on PROJECT-ID from REMOTE-FILE to LOCAL-FILE.
The update is sent through Overleaf's real-time ShareJS text OT path, so
the remote document id and Overleaf edit history are preserved."
  (overleaf-project--update-doc-text-content
   project-id
   doc-id
   (overleaf-project--file-utf8-string remote-file)
   (overleaf-project--file-utf8-string local-file)))

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
