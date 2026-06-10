;;; overleaf-project-http-auth-test.el --- HTTP/auth boundary tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'url-util)
(require 'overleaf-project-auth)
(require 'overleaf-project-core)
(require 'overleaf-project-http)

(ert-deftest overleaf-project-http-auth-test-csrf-token-cache-and-failure ()
  (let ((overleaf-project-url "https://example.overleaf.test")
        (overleaf-project--csrf-cache (make-hash-table :test #'equal))
        (overleaf-project-cookies "sid=1")
        (overleaf-project--current-cookies nil)
        (requests nil))
    (cl-letf (((symbol-function 'overleaf-project--curl-request)
               (lambda (method url headers &optional body)
                 (push (list method url headers body) requests)
                 "<html><meta name=\"ol-csrfToken\" content=\"token-1\"></html>")))
      (should (equal (overleaf-project--csrf-token "project-id")
                     "token-1"))
      (should (equal (overleaf-project--csrf-token "project-id")
                     "token-1"))
      (should (= (length requests) 1))
      (should (equal (caar requests) "GET")))
    (cl-letf (((symbol-function 'overleaf-project--curl-request)
               (lambda (&rest _args) "<html></html>")))
      (should-error (overleaf-project--csrf-token "other-project")
                    :type 'user-error))))

(ert-deftest overleaf-project-http-auth-test-socket-cookies-appends-gclb ()
  (let ((overleaf-project-url "https://example.overleaf.test")
        (overleaf-project-cookies "sid=1")
        (overleaf-project--current-cookies nil))
    (cl-letf (((symbol-function 'overleaf-project--curl-header-text)
               (lambda (&rest _args)
                 "HTTP/1.1 200 OK\r\nSet-Cookie: GCLB=abc; Path=/\r\n")))
      (should (equal (overleaf-project--socket-cookies)
                     "sid=1; GCLB=abc")))
    (cl-letf (((symbol-function 'overleaf-project--curl-header-text)
               (lambda (&rest _args) "HTTP/1.1 200 OK\r\n")))
      (should (equal (overleaf-project--socket-cookies) "sid=1")))))

(ert-deftest overleaf-project-http-auth-test-curl-upload-retries-after-403 ()
  (let ((overleaf-project-url "https://example.overleaf.test")
        (overleaf-project-cookies "sid=1")
        (overleaf-project--current-cookies nil)
        (overleaf-project--csrf-cache (make-hash-table :test #'equal))
        (overleaf-project-curl-executable "curl")
        (runs nil)
        (warnings nil))
    (cl-letf (((symbol-function 'overleaf-project--csrf-token)
               (lambda (_project-id &optional _refresh) "csrf-token"))
              ((symbol-function 'overleaf-project--clear-csrf-cache)
               (lambda (&optional project-id)
                 (push (list :clear-csrf project-id) warnings)))
              ((symbol-function 'overleaf-project--warn)
               (lambda (&rest args)
                 (push (cons :warn args) warnings)))
              ((symbol-function 'overleaf-project--ensure-executable)
               (lambda (program) program))
              ((symbol-function 'overleaf-project--run)
               (lambda (program args &optional _directory _env noerror)
                 (push (list program args noerror) runs)
                 (if (= (length runs) 1)
                     (make-overleaf-project--command-result
                      :status 22
                      :output "curl: returned error: 403")
                   (make-overleaf-project--command-result
                    :status 0
                    :output "{\"entity_id\":\"entity-1\",\"entity_type\":\"doc\"}")))))
      (should (equal (overleaf-project--curl-upload-file
                      "project-id"
                      "folder-id"
                      "main.tex"
                      "/tmp/main.tex")
                     '(:entity_id "entity-1" :entity_type "doc")))
      (should (= (length runs) 2))
      (should (member '(:clear-csrf "project-id") warnings)))))

(ert-deftest overleaf-project-http-auth-test-curl-upload-redacts-final-error ()
  (let ((overleaf-project-url "https://example.overleaf.test")
        (overleaf-project-cookies "sid=secret")
        (overleaf-project--current-cookies nil)
        (overleaf-project-curl-executable "curl"))
    (cl-letf (((symbol-function 'overleaf-project--csrf-token)
               (lambda (&rest _args) "csrf-secret"))
              ((symbol-function 'overleaf-project--ensure-executable)
               (lambda (program) program))
              ((symbol-function 'overleaf-project--run)
               (lambda (_program _args &optional _directory _env _noerror)
                 (make-overleaf-project--command-result
                  :status 22
                  :output "Cookie: sid=secret\nX-Csrf-Token: csrf-secret"))))
      (let ((err (should-error
                  (overleaf-project--curl-upload-file
                   "project-id"
                   "folder-id"
                   "main.tex"
                   "/tmp/main.tex")
                  :type 'error)))
        (should-not (string-match-p "sid=secret\\|csrf-secret"
                                    (error-message-string err)))
        (should (string-match-p "<redacted>"
                                (error-message-string err)))))))

(ert-deftest overleaf-project-http-auth-test-project-list-parses-prefetched-blob ()
  (let* ((overleaf-project-url "https://example.overleaf.test")
         (overleaf-project-cookies "sid=1")
         (overleaf-project--current-cookies nil)
         (json "{\"projects\":[{\"id\":\"p1\",\"name\":\"One\"}]}")
         (encoded (url-hexify-string json))
         (html (format "<input name=\"ol-prefetchedProjectsBlob\" content=\"%s\">"
                       encoded)))
    (cl-letf (((symbol-function 'overleaf-project--curl-request)
               (lambda (&rest _args) html)))
      (should (equal (overleaf-project-list)
                     '((:id "p1" :name "One")))))
    (cl-letf (((symbol-function 'overleaf-project--curl-request)
               (lambda (&rest _args) "<html></html>")))
      (should-error (overleaf-project-list)
                    :type 'user-error))))

(ert-deftest overleaf-project-http-auth-test-create-folder-and-delete-entity ()
  (let ((requests nil))
    (cl-letf (((symbol-function 'overleaf-project--url)
               (lambda () "https://example.overleaf.test"))
              ((symbol-function 'overleaf-project--project-headers)
               (lambda (&rest _args) '(("Cookie" . "sid=1"))))
              ((symbol-function 'overleaf-project--curl-request)
               (lambda (method url headers &optional body)
                 (push (list method url headers body) requests)
                 "{\"_id\":\"folder-id\",\"name\":\"New\"}")))
      (should (equal (overleaf-project--create-folder
                      "project-id"
                      "root"
                      "New")
                     '(:_id "folder-id" :name "New")))
      (let ((request (car requests)))
        (should (equal (nth 0 request) "POST"))
        (should (equal (nth 1 request)
                       "https://example.overleaf.test/project/project-id/folder"))
        (should (string-match-p "parent_folder_id" (nth 3 request))))))
  (let ((requests nil))
    (cl-letf (((symbol-function 'overleaf-project--url)
               (lambda () "https://example.overleaf.test"))
              ((symbol-function 'overleaf-project--project-headers)
               (lambda (&rest _args) '(("Cookie" . "sid=1"))))
              ((symbol-function 'overleaf-project--curl-request)
               (lambda (method url headers &optional body)
                 (push (list method url headers body) requests)
                 "")))
      (overleaf-project--delete-entity
       "project-id"
       (make-overleaf-project--entity
        :id "doc-id"
        :type 'doc))
      (should (equal (nth 0 (car requests)) "DELETE"))
      (should (equal (nth 1 (car requests))
                     "https://example.overleaf.test/project/project-id/doc/doc-id"))
      (should-error
       (overleaf-project--delete-entity
        "project-id"
        (make-overleaf-project--entity :id "bad" :type 'unknown))
       :type 'user-error))))

(ert-deftest overleaf-project-http-auth-test-auth-cookie-apply-and-save ()
  (let ((overleaf-project-url "https://www.overleaf.com")
        (overleaf-project--current-cookies nil)
        (overleaf-project--csrf-cache (make-hash-table :test #'equal))
        (saved nil)
        (messages nil))
    (puthash "x" "csrf" overleaf-project--csrf-cache)
    (cl-letf (((symbol-function 'overleaf-project--message)
               (lambda (&rest args) (push args messages))))
      (overleaf-project--apply-authenticated-cookies
       '(("www.overleaf.com" "sid=1" nil))
       "Saved cookies for %s")
      (should (= (hash-table-count overleaf-project--csrf-cache) 0))
      (should (equal overleaf-project--current-cookies
                     '(("www.overleaf.com" "sid=1" nil))))
      (should (equal (car messages)
                     '("Saved cookies for %s" "www.overleaf.com"))))
    (let ((overleaf-project-save-cookies
           (lambda (value) (setq saved value))))
      (should (equal (overleaf-project--save-and-apply-authenticated-cookies
                      '(("www.overleaf.com" "sid=2" nil))
                      nil)
                     '(("www.overleaf.com" "sid=2" nil))))
      (should (equal saved "((\"www.overleaf.com\" \"sid=2\" nil))"))))
  (let ((overleaf-project-save-cookies nil))
    (should-error
     (overleaf-project--save-and-apply-authenticated-cookies
      '(("www.overleaf.com" "sid=1" nil))
      nil)
     :type 'user-error)))

(ert-deftest overleaf-project-http-auth-test-authenticate-sync-selects-backend ()
  (let ((calls nil))
    (cl-letf (((symbol-function 'overleaf-project--authenticate-with-webdriver)
               (lambda (&optional url) (push (list 'webdriver url) calls)))
              ((symbol-function 'overleaf-project--authenticate-with-firefox-cookies)
               (lambda (&optional url) (push (list 'firefox url) calls))))
      (let ((overleaf-project-auth-backend 'webdriver))
        (overleaf-project--authenticate-sync "https://one.test"))
      (let ((overleaf-project-auth-backend 'firefox-cookies))
        (overleaf-project--authenticate-sync "https://two.test"))
      (should (equal (nreverse calls)
                     '((webdriver "https://one.test")
                       (firefox "https://two.test")))))
    (let ((overleaf-project-auth-backend 'unknown))
      (should-error (overleaf-project--authenticate-sync)
                    :type 'user-error))))

(ert-deftest overleaf-project-http-auth-test-webdriver-cookie-helpers ()
  (let* ((cookies (vector '((name . "connect.sid")
                            (value . "session")
                            (expiry . 100))
                          '((name . "GCLB")
                            (value . "lb")
                            (expiry . 50)))))
    (should (equal (overleaf-project--webdriver-cookie-string cookies)
                   "connect.sid=session; GCLB=lb"))
    (should (equal (overleaf-project--webdriver-cookie-expiry cookies)
                   100)))
  (should-error (overleaf-project--webdriver-cookie-string [])
                :type 'user-error)
  (let ((overleaf-project-url "https://example.overleaf.test"))
    (should (equal (overleaf-project--webdriver-project-url "/project/abc")
                   "https://example.overleaf.test/project/abc"))
    (should (equal (overleaf-project--webdriver-project-url
                    "https://other.test/project/abc")
                   "https://other.test/project/abc"))))

;;; overleaf-project-http-auth-test.el ends here
