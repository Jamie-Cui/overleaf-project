;;; overleaf-project-test.el --- Tests for overleaf-project -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'overleaf-project-core)
(require 'overleaf-project-http)
(require 'overleaf-project-sync)
(require 'overleaf-project-firefox)

(defmacro overleaf-project-test--with-url (url &rest body)
  "Run BODY with `overleaf-project-url' bound to URL."
  (declare (indent 1) (debug t))
  `(let ((overleaf-project-url ,url))
     ,@body))

(defmacro overleaf-project-test--with-temp-dir (var &rest body)
  "Bind VAR to a temporary directory while running BODY."
  (declare (indent 1) (debug t))
  `(let ((,var (make-temp-file "overleaf-project-test." t)))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-directory ,var t)))))

(ert-deftest overleaf-project-test-url-helpers ()
  (overleaf-project-test--with-url " https://Example.Overleaf.test/ "
    (should (equal (overleaf-project--url)
                   "https://Example.Overleaf.test"))
    (should (equal (overleaf-project--url-host)
                   "example.overleaf.test"))
    (should (equal (overleaf-project--cookie-domain)
                   "example.overleaf.test"))
    (should (equal (overleaf-project--project-page-url "abc123")
                   "https://Example.Overleaf.test/project/abc123")))
  (overleaf-project-test--with-url "not a url"
    (should-error (overleaf-project--url-host) :type 'user-error)))

(ert-deftest overleaf-project-test-cookie-key-candidates ()
  (overleaf-project-test--with-url "https://www.overleaf.com"
    (should (equal (overleaf-project--cookie-key-candidates)
                   '("www.overleaf.com"
                     ".www.overleaf.com"
                     "overleaf.com"
                     ".overleaf.com"))))
  (overleaf-project-test--with-url "https://overleaf.example"
    (should (equal (overleaf-project--cookie-key-candidates)
                   '("overleaf.example" ".overleaf.example")))))

(ert-deftest overleaf-project-test-sanitize-name ()
  (should (equal (overleaf-project--sanitize-name "  My Project v2.tex!! ")
                 "my-project-v2-tex"))
  (should (equal (overleaf-project--sanitize-name "---") "")))

(ert-deftest overleaf-project-test-redact-command-data ()
  (should (equal (overleaf-project--redact-sensitive-argument
                  "Cookie: session=secret")
                 "Cookie: <redacted>"))
  (should (equal (overleaf-project--redact-command-args
                  '("-H" "Cookie: session=secret"
                    "--header" "X-Csrf-Token: token"
                    "--cookie" "raw-cookie-secret"
                    "-H" "Accept: application/json"
                    "https://example.test"))
                 '("-H" "Cookie: <redacted>"
                   "--header" "X-Csrf-Token: <redacted>"
                   "--cookie" "<redacted>"
                   "-H" "Accept: application/json"
                   "https://example.test")))
  (let ((message (overleaf-project--command-error-message
                  "curl"
                  '("-H" "Cookie: session=secret" "https://example.test")
                  "X-Csrf-Token: secret-token-value\nbody")))
    (should (string-match-p "Cookie: <redacted>" message))
    (should (string-match-p "X-Csrf-Token: <redacted>" message))
    (should-not (string-match-p "session=secret\\|secret-token-value"
                                message))))

(ert-deftest overleaf-project-test-normalize-cookies ()
  (overleaf-project-test--with-url "https://www.overleaf.com"
    (should (equal (overleaf-project--normalize-cookie-entry
                    '("WWW.OVERLEAF.COM" "sid=1" 123))
                   '("www.overleaf.com" "sid=1" 123)))
    (should (equal (overleaf-project--normalize-cookie-entry
                    '("www.overleaf.com" "sid=1"))
                   '("www.overleaf.com" "sid=1" nil)))
    (should-error (overleaf-project--normalize-cookie-entry
                   '("www.overleaf.com" 42))
                  :type 'error)
    (should (equal (overleaf-project--normalize-full-cookies "sid=1")
                   '(("www.overleaf.com" "sid=1" nil))))
    (should (equal (overleaf-project--normalize-full-cookies
                    "((\".www.overleaf.com\" \"sid=1\" 999))")
                   '((".www.overleaf.com" "sid=1" 999))))
    (should (equal (overleaf-project--normalize-full-cookies "  ")
                   nil))
    (should-error (overleaf-project--normalize-full-cookies
                   "((\"broken\" 42))")
                  :type 'error)
    (should-error (overleaf-project--normalize-full-cookies 42)
                  :type 'error)))

(ert-deftest overleaf-project-test-cookie-state ()
  (let* ((now (time-convert nil 'integer))
         (overleaf-project--current-cookies nil)
         (overleaf-project-cookies
          `(("overleaf.com" "sid=valid" ,(+ now 3600)))))
    (overleaf-project-test--with-url "https://www.overleaf.com"
      (should (equal (plist-get (overleaf-project--cookie-state) :status)
                     'valid))
      (should (equal (overleaf-project--get-cookies) "sid=valid"))))
  (let* ((now (time-convert nil 'integer))
         (overleaf-project--current-cookies nil)
         (overleaf-project-cookies
          `(("www.overleaf.com" "sid=expired" ,(- now 3600)))))
    (overleaf-project-test--with-url "https://www.overleaf.com"
      (should (equal (plist-get (overleaf-project--cookie-state) :status)
                     'expired))
      (should-error (overleaf-project--get-cookies) :type 'user-error)))
  (let ((overleaf-project--current-cookies nil)
        (overleaf-project-cookies nil))
    (overleaf-project-test--with-url "https://www.overleaf.com"
      (should (equal (plist-get (overleaf-project--cookie-state) :status)
                     'missing))
      (should-error (overleaf-project--get-cookies) :type 'user-error)
      (should (string-match-p
               "not set locally"
               (overleaf-project--authentication-needed-reason))))))

(ert-deftest overleaf-project-test-curl-helper-arguments ()
  (should (equal (overleaf-project--format-curl-headers
                  '(("A" . "1") ("B" . "2")))
                 '("A: 1" "B: 2")))
  (let ((overleaf-project--curl-connect-timeout 2)
        (overleaf-project--curl-max-time nil))
    (should (equal (overleaf-project--curl-timeout-args)
                   '("--connect-timeout" "2"))))
  (let ((overleaf-project--curl-connect-timeout nil)
        (overleaf-project--curl-download-max-time 10)
        (overleaf-project--curl-download-speed-limit 256)
        (overleaf-project--curl-download-speed-time 5))
    (should (equal (overleaf-project--curl-download-timeout-args)
                   '("--max-time" "10"
                     "--speed-limit" "256"
                     "--speed-time" "5"))))
  (let ((result (make-overleaf-project--command-result
                 :status 22
                 :output "curl: returned error: 403")))
    (should (overleaf-project--curl-403-p result)))
  (let ((result (make-overleaf-project--command-result
                 :status 0
                 :output "")))
    (should-not (overleaf-project--curl-403-p result))))

(ert-deftest overleaf-project-test-csrf-cache-helpers ()
  (let ((overleaf-project--csrf-cache (make-hash-table :test #'equal)))
    (overleaf-project-test--with-url "https://www.overleaf.com"
      (let ((key-a (overleaf-project--csrf-cache-key "project-a" "cookie-a"))
            (key-b (overleaf-project--csrf-cache-key "project-b" "cookie-b")))
        (puthash key-a "token-a" overleaf-project--csrf-cache)
        (puthash key-b "token-b" overleaf-project--csrf-cache)
        (overleaf-project--clear-csrf-cache "project-a")
        (should-not (gethash key-a overleaf-project--csrf-cache))
        (should (equal (gethash key-b overleaf-project--csrf-cache)
                       "token-b"))
        (overleaf-project--clear-csrf-cache)
        (should (= (hash-table-count overleaf-project--csrf-cache) 0))))))

(ert-deftest overleaf-project-test-sync-metadata-path-validation ()
  (let ((overleaf-project-sync-metadata-enabled t)
        (overleaf-project-sync-metadata-file ".overleaf-project-sync.json"))
    (should (equal (overleaf-project--sync-metadata-relative-path)
                   ".overleaf-project-sync.json"))
    (should (overleaf-project--sync-metadata-path-p
             ".overleaf-project-sync.json")))
  (let ((overleaf-project-sync-metadata-enabled nil)
        (overleaf-project-sync-metadata-file ".overleaf-project-sync.json"))
    (should-not (overleaf-project--sync-metadata-relative-path))
    (should-not (overleaf-project--sync-metadata-path-p
                 ".overleaf-project-sync.json")))
  (dolist (path '("" "/absolute" "dir/file" ".."))
    (let ((overleaf-project-sync-metadata-enabled t)
          (overleaf-project-sync-metadata-file path))
      (should-error (overleaf-project--sync-metadata-relative-path)
                    :type 'user-error))))

(ert-deftest overleaf-project-test-sync-metadata-file-reading ()
  (let ((overleaf-project-log-echo nil))
    (overleaf-project-test--with-temp-dir dir
      (let ((valid (expand-file-name "valid.json" dir))
            (invalid (expand-file-name "invalid.json" dir)))
        (with-temp-file valid
          (insert "{\"schema\":1,\"localCommit\":\"abc\"}"))
        (with-temp-file invalid
          (insert "{not-json"))
        (should (equal (overleaf-project--read-sync-metadata-file valid)
                       '(:schema 1 :localCommit "abc")))
        (should-not (overleaf-project--read-sync-metadata-file invalid))))))

(ert-deftest overleaf-project-test-git-object-id-p ()
  (should (overleaf-project--git-object-id-p
           "0123456789abcdef0123456789abcdef01234567"))
  (should (overleaf-project--git-object-id-p
           "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"))
  (should-not (overleaf-project--git-object-id-p "short"))
  (should-not (overleaf-project--git-object-id-p "not-a-sha")))

(ert-deftest overleaf-project-test-classify-sync-state ()
  (should (eq (overleaf-project--classify-sync-state "a" "a" "a")
              'in-sync))
  (should (eq (overleaf-project--classify-sync-state "a" "b" "b")
              'head-matches-remote))
  (should (eq (overleaf-project--classify-sync-state "a" "b" "a")
              'remote-matches-base))
  (should (eq (overleaf-project--classify-sync-state "a" "a" "b")
              'head-matches-base))
  (should (eq (overleaf-project--classify-sync-state "a" "b" "c")
              'diverged)))

(ert-deftest overleaf-project-test-sharejs-text-op ()
  (should-not (overleaf-project--sharejs-text-op "same" "same"))
  (should (equal (overleaf-project--sharejs-text-op "ab" "aXb")
                 [(:i "X" :p 1)]))
  (should (equal (overleaf-project--sharejs-text-op "aXb" "ab")
                 [(:d "X" :p 1)]))
  (should (equal (overleaf-project--sharejs-text-op "ab" "aXYb")
                 [(:i "XY" :p 1)]))
  (should (equal (overleaf-project--sharejs-text-op "aXb" "aYb")
                 [(:d "X" :p 1) (:i "Y" :p 1)]))
  (should (equal (overleaf-project--sharejs-text-op
                  "a\U0001F600b"
                  "a\U0001F600Xb")
                 [(:i "X" :p 3)])))

(ert-deftest overleaf-project-test-socketio-doc-line-decoding ()
  (should (equal (overleaf-project--decode-socketio-doc-line "hello" "doc")
                 "hello"))
  (should-error (overleaf-project--decode-socketio-doc-line
                 (string #x100)
                 "doc")
                :type 'user-error)
  (should-error (overleaf-project--decode-socketio-doc-line
                 (string #xff)
                 "doc")
                :type 'user-error))

(ert-deftest overleaf-project-test-socketio-event-helpers ()
  (should (equal (overleaf-project--socketio-error-message nil) nil))
  (should (equal (overleaf-project--socketio-error-message "boom") "boom"))
  (should (equal (overleaf-project--socketio-error-message '(:message "boom"))
                 "boom"))
  (should (equal (overleaf-project--doc-update-event-doc-id
                  '(:name "otUpdateApplied" :args ((:doc "doc-a"))))
                 "doc-a"))
  (should (equal (overleaf-project--doc-update-event-doc-id
                  '(:name "otUpdateError" :args (nil (:doc_id "doc-b"))))
                 "doc-b"))
  (should (overleaf-project--source-doc-update-applied-p
           '(:name "otUpdateApplied" :args ((:doc "doc-a")))
           "doc-a"))
  (should-not (overleaf-project--source-doc-update-applied-p
               '(:name "otUpdateApplied" :args ((:doc "doc-a" :op [])))
               "doc-a")))

(ert-deftest overleaf-project-test-remote-doc-state-validation ()
  (cl-letf (((symbol-function 'overleaf-project--socketio-emit)
             (lambda (&rest _args)
               '(nil ("line1" "line2") 7 nil nil "sharejs-text-ot"))))
    (should (equal (overleaf-project--remote-doc-state nil "doc")
                   '(:version 7 :text "line1\nline2"))))
  (cl-letf (((symbol-function 'overleaf-project--socketio-emit)
             (lambda (&rest _args)
               '("join failed" nil nil nil nil nil))))
    (should-error (overleaf-project--remote-doc-state nil "doc")
                  :type 'user-error))
  (cl-letf (((symbol-function 'overleaf-project--socketio-emit)
             (lambda (&rest _args)
               '(nil ("line") 7 nil nil "other-type"))))
    (should-error (overleaf-project--remote-doc-state nil "doc")
                  :type 'user-error))
  (cl-letf (((symbol-function 'overleaf-project--socketio-emit)
             (lambda (&rest _args)
               '(nil ("line") nil nil nil "sharejs-text-ot"))))
    (should-error (overleaf-project--remote-doc-state nil "doc")
                  :type 'user-error)))

(ert-deftest overleaf-project-test-entity-table-helpers ()
  (let* ((root '(:name "rootFolder"
                 :_id "root"
                 :docs ((:name "main.tex" :_id "doc-1"))
                 :fileRefs ((:name "figure.png" :_id "file-1"))
                 :folders ((:name "chapters"
                             :_id "folder-1"
                             :docs ((:name "intro.tex" :_id "doc-2"))))))
         (table (overleaf-project--build-entity-table root)))
    (should (equal (overleaf-project--entity-type (gethash "" table))
                   'folder))
    (should (equal (overleaf-project--entity-id (gethash "main.tex" table))
                   "doc-1"))
    (should (equal (overleaf-project--entity-type (gethash "figure.png" table))
                   'file))
    (should (equal (overleaf-project--entity-parent-id
                    (gethash "chapters/intro.tex" table))
                   "folder-1"))
    (overleaf-project--forget-entry table "chapters")
    (should-not (gethash "chapters" table))
    (should-not (gethash "chapters/intro.tex" table))
    (should (gethash "main.tex" table))))

(ert-deftest overleaf-project-test-firefox-profiles-ini ()
  (overleaf-project-test--with-temp-dir dir
    (let ((ini (expand-file-name "profiles.ini" dir)))
      (with-temp-file ini
        (insert "[Profile0]\n")
        (insert "Name=default\n")
        (insert "IsRelative=1\n")
        (insert "Path=Profiles/default-release\n")
        (insert "Default=1\n")
        (insert "\n[InstallABC]\n")
        (insert "Default=Profiles/install-default\n"))
      (let* ((sections (overleaf-project-firefox--parse-profiles-ini ini))
             (default (overleaf-project-firefox--default-profile-section
                        sections)))
        (should (equal (plist-get (car sections) :section) "Profile0"))
        (should (equal (plist-get default :path)
                       "Profiles/default-release"))
        (should (equal (overleaf-project-firefox--resolve-profile-path
                        default
                        dir)
                       (expand-file-name "Profiles/default-release" dir))))))
  (let ((install-only '((:section "InstallABC"
                        :default "Profiles/install-default"))))
    (should (equal (plist-get
                    (overleaf-project-firefox--default-profile-section
                     install-only)
                    :path)
                   "Profiles/install-default")))
  (should-error (overleaf-project-firefox--resolve-profile-path
                 '(:section "Profile0")
                 temporary-file-directory)
                :type 'user-error))

(ert-deftest overleaf-project-test-firefox-cookie-rows ()
  (should (equal (overleaf-project-firefox--cookie-query '("a" "b" "c"))
                 "select name, value, host, path, expiry from moz_cookies where host in (?, ?, ?)"))
  (should (overleaf-project-firefox--cookie-expired-p
           '("connect.sid" "value" ".overleaf.com" "/" 10)
           10))
  (should-not (overleaf-project-firefox--cookie-expired-p
               '("connect.sid" "value" ".overleaf.com" "/" 0)
               10))
  (should (overleaf-project-firefox--session-cookie-p
           '("connect.sid" "value" ".overleaf.com" "/" 100)))
  (should-not (overleaf-project-firefox--session-cookie-p
               '("GCLB" "value" ".overleaf.com" "/" 100)))
  (should (equal (overleaf-project-firefox--cookie-header
                  '(("connect.sid" "session" ".overleaf.com" "/" 100)
                    ("GCLB" "lb" ".overleaf.com" "/" 100)
                    (nil "ignored" ".overleaf.com" "/" 100)))
                 "connect.sid=session; GCLB=lb"))
  (should-error (overleaf-project-firefox--cookie-header
                 '((nil "ignored" ".overleaf.com" "/" 100)))
                :type 'user-error)
  (should (equal (overleaf-project-firefox--session-expiry
                  '(("connect.sid" "a" ".overleaf.com" "/" 50)
                    ("overleaf_session2" "b" ".overleaf.com" "/" 40)
                    ("GCLB" "lb" ".overleaf.com" "/" 10)))
                 40)))

(ert-deftest overleaf-project-test-firefox-full-cookies-from-rows ()
  (let* ((now (time-convert nil 'integer))
         (rows `(("connect.sid" "session" ".www.overleaf.com" "/" ,(+ now 100))
                 ("GCLB" "lb" ".www.overleaf.com" "/" ,(+ now 50)))))
    (overleaf-project-test--with-url "https://www.overleaf.com"
      (should (equal (overleaf-project-firefox--full-cookies-from-rows
                      rows
                      "/tmp/profile")
                     `(("www.overleaf.com"
                        "connect.sid=session; GCLB=lb"
                        ,(+ now 100)))))))
  (let ((now (time-convert nil 'integer)))
    (overleaf-project-test--with-url "https://www.overleaf.com"
      (should-error (overleaf-project-firefox--full-cookies-from-rows
                     nil
                     "/tmp/profile")
                    :type 'user-error)
      (should-error (overleaf-project-firefox--full-cookies-from-rows
                     `(("GCLB" "lb" ".www.overleaf.com" "/" ,(+ now 100)))
                     "/tmp/profile")
                    :type 'user-error)
      (should-error (overleaf-project-firefox--full-cookies-from-rows
                     `(("connect.sid" "session" ".www.overleaf.com" "/"
                        ,(- now 100)))
                     "/tmp/profile")
                    :type 'user-error))))

;;; overleaf-project-test.el ends here
