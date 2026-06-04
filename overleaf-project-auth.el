;;; overleaf-project-auth.el --- Browser authentication for overleaf-project -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Overleaf authentication backends.

;;; Code:

(require 'cl-lib)
(require 'url-expand)
(require 'webdriver)
(require 'webdriver-firefox)
(require 'overleaf-project-core)
(require 'overleaf-project-firefox)
(require 'overleaf-project-http)

(declare-function overleaf-project--async-enabled-p "overleaf-project-core")
(declare-function overleaf-project--async-register-process "overleaf-project-core")
(declare-function overleaf-project--async-unregister-process "overleaf-project-core")

;;;; Authentication

(defmacro overleaf-project--with-webdriver (&rest body)
  "Execute BODY if geckodriver is available."
  `(if (not (executable-find "geckodriver"))
       (progn
         (message-box
          "Please install geckodriver to authenticate with Overleaf.")
         (user-error "Required executable `geckodriver' was not found"))
     ,@body))

;; `webdriver-firefox' currently assumes geckodriver immediately prints a
;; specific "Listening on ..." line before `webdriver-service-start' asks for
;; the port.  Newer geckodriver builds can race with that lookup, which raises
;; a plain `search-failed' before authentication even opens Firefox.  Poll the
;; process buffer briefly and fall back to the already configured port.
(cl-defmethod webdriver-service-get-port ((self webdriver-service-firefox))
  "Return the port where SELF is listening, tolerating delayed log output."
  (let ((process (oref self process))
        (buffer (get-buffer (oref self buffer)))
        (fallback (oref self port))
        (deadline (+ (float-time) 3.0))
        port)
    (when buffer
      (with-current-buffer buffer
        (save-excursion
          (while (and (not port)
                      (process-live-p process)
                      (< (float-time) deadline))
            (goto-char (point-max))
            (when (re-search-backward
                   "Listening on .*:\\([0-9]+\\)\\(?:[[:space:]]*$\\|/\\)"
                   nil t)
              (setq port (string-to-number (match-string 1))))
            (unless port
              (accept-process-output process 0.1 nil t))))))
    (or port
        (and (integerp fallback) fallback)
        (error "Could not determine geckodriver listening port"))))

(defun overleaf-project--webdriver-service ()
  "Return a Firefox webdriver service using an available local port."
  (make-instance 'webdriver-service-firefox
                 :port (webdriver--get-free-port)
                 :buffer (generate-new-buffer
                          " *overleaf-project-geckodriver*")))

(defun overleaf-project--webdriver-session ()
  "Return a webdriver session for Overleaf authentication."
  (make-instance 'webdriver-session
                 :service (overleaf-project--webdriver-service)))

(defun overleaf-project--webdriver-session-stop (session)
  "Stop SESSION and its service without masking an earlier error."
  (let ((service (oref session service)))
    (condition-case err
        (when (oref session id)
          (webdriver-session-stop session))
      (error
       (overleaf-project--debug
        "Ignoring webdriver session cleanup error: %s"
        (error-message-string err))))
    (when service
      (condition-case err
          (webdriver-service-stop service)
        (error
         (overleaf-project--debug
          "Ignoring webdriver service cleanup error: %s"
          (error-message-string err)))))))

(defmacro overleaf-project--with-webdriver-direct-connection (&rest body)
  "Run BODY with local webdriver HTTP requests bypassing proxies."
  (declare (indent 0) (debug t))
  `(let ((url-proxy-services
          (cons '("no_proxy" . "\\`\\(?:localhost\\|127\\.0\\.0\\.1\\)\\'")
                (assq-delete-all "no_proxy" (copy-sequence url-proxy-services)))))
     ,@body))

(cl-defmacro overleaf-project--webdriver-wait-until-appears
    ((session xpath &optional (element-sym '_unused) (delay .1)) &rest body)
  "Wait until XPATH appears in SESSION, bind it to ELEMENT-SYM and run BODY."
  (let ((not-found (gensym))
        (selector (gensym)))
    `(let ((,selector
            (make-instance 'webdriver-by
                           :strategy "xpath"
                           :selector ,xpath))
           (,not-found t))
       (while ,not-found
         (condition-case nil
             (let ((,element-sym
                    (webdriver-find-element ,session ,selector)))
               (setq ,not-found nil)
               ,@body)
           (webdriver-error
            (sleep-for ,delay)))))))

(defun overleaf-project--webdriver-cookie-string (cookies)
  "Return an HTTP Cookie header string for webdriver COOKIES."
  (let ((pairs
         (cl-loop
          for cookie across cookies
          for name = (alist-get 'name cookie)
          for value = (alist-get 'value cookie)
          when (and (stringp name) (stringp value))
          collect (format "%s=%s" name value))))
    (unless pairs
      (user-error "No cookies were captured after Overleaf authentication"))
    (string-join pairs "; ")))

(defun overleaf-project--webdriver-project-url (href)
  "Return an absolute Overleaf project URL for HREF.
HREF may already be absolute or may be a relative path such as
\"/project/...\"."
  (and (stringp href)
       (url-expand-file-name href (concat (overleaf-project--url) "/"))))

(defun overleaf-project--webdriver-cookie-expiry (cookies)
  "Return the authenticated-session expiry for webdriver COOKIES, or nil.
Only cookie names matching `overleaf-project-auth-session-cookie-regexp' are
considered.  This avoids treating short-lived analytics cookies as the
expiry of the actual Overleaf login session."
  (let ((expiries
         (cl-loop
          for cookie across cookies
          for name = (alist-get 'name cookie)
          for expiry = (alist-get 'expiry cookie)
          when (and (stringp name)
                    (integerp expiry)
                    (string-match-p
                     overleaf-project-auth-session-cookie-regexp
                     name))
          collect expiry)))
    (when expiries
      (apply #'min expiries))))

(defun overleaf-project--apply-authenticated-cookies (full-cookies message)
  "Apply FULL-COOKIES to the current session and display MESSAGE.
MESSAGE is formatted with the current cookie domain when non-nil."
  (overleaf-project--clear-csrf-cache)
  (setq overleaf-project--current-cookies
        (overleaf-project--normalize-full-cookies full-cookies))
  (when message
    (overleaf-project--message message (overleaf-project--cookie-domain))))

(defun overleaf-project--save-and-apply-authenticated-cookies
    (full-cookies message)
  "Save and apply FULL-COOKIES, then display MESSAGE."
  (unless (and (boundp 'overleaf-project-save-cookies)
               overleaf-project-save-cookies)
    (user-error
     "`overleaf-project-save-cookies' needs to be configured"))
  (funcall overleaf-project-save-cookies (prin1-to-string full-cookies))
  (overleaf-project--apply-authenticated-cookies full-cookies message)
  full-cookies)

(defun overleaf-project--authenticate-with-firefox-cookies (&optional url)
  "Synchronously import Overleaf cookies from Firefox for URL."
  (setq overleaf-project-url (or url (overleaf-project--url)))
  (overleaf-project--save-and-apply-authenticated-cookies
   (overleaf-project-firefox-cookies overleaf-project-url)
   "Imported Overleaf cookies from Firefox for %s"))

(defun overleaf-project--start-authentication-async
    (&optional url on-success)
  "Start authentication for URL in the background.
ON-SUCCESS, when non-nil, is called after cookies are saved and applied."
  (when (and (eq overleaf-project-auth-backend 'webdriver)
             (not (executable-find "geckodriver")))
    (message-box
     "Please install geckodriver to authenticate with Overleaf.")
    (user-error "Required executable `geckodriver' was not found"))
  (setq overleaf-project-url (or url (overleaf-project--url)))
  (overleaf-project--async-start
   (format "Overleaf authentication for %s" (overleaf-project--url-host))
   (lambda ()
     (overleaf-project--authenticate-1 overleaf-project-url))
   :key (format "auth:%s" (overleaf-project--url))
   :on-success
   (lambda (full-cookies)
     (overleaf-project--apply-authenticated-cookies full-cookies nil)
     (overleaf-project--message "Authentication finished for %s"
                                (overleaf-project--cookie-domain))
     (when on-success
       (funcall on-success full-cookies)))))

(defun overleaf-project--authenticate-with-webdriver (&optional url)
  "Synchronously use selenium webdriver to log into URL and obtain cookies.
If URL is nil, use `overleaf-project-url'.  Return the saved full cookie alist."
  (overleaf-project--with-webdriver
   (unless (and (boundp 'overleaf-project-save-cookies)
                overleaf-project-save-cookies)
     (user-error
      "`overleaf-project-save-cookies' needs to be configured"))
   (setq overleaf-project-url (or url (overleaf-project--url)))
   (let ((session (overleaf-project--webdriver-session)))
     (unwind-protect
         ;; Re-authentication should not depend on previously saved cookies.
         ;; Using only the freshly captured cookie avoids failures from stale
         ;; or undecryptable cookie stores.
         (overleaf-project--with-webdriver-direct-connection
           (let ((full-cookies nil))
             (webdriver-service-start (oref session service))
             (overleaf-project--async-register-process
              (oref (oref session service) process))
             (webdriver-session-start session)
             (webdriver-goto-url session (concat (overleaf-project--url) "/login"))
             (overleaf-project--message "Log in using the browser window...")
             (overleaf-project--webdriver-wait-until-appears
              (session "//button[@id='new-project-button-sidebar']"))
             (let* ((project-link-selector
                     (make-instance 'webdriver-by
                                    :strategy "xpath"
                                    :selector "//a[contains(@href, '/project/')]"))
                    (first-project
                     (ignore-errors
                       (webdriver-find-element session project-link-selector)))
                    (first-project-path
                     (and first-project
                          (overleaf-project--webdriver-project-url
                           (webdriver-get-element-attribute
                            session
                            first-project
                            "href"))))
                    (cookies nil))
               (when first-project-path
                 (webdriver-goto-url session first-project-path))
               (setq cookies (webdriver-get-all-cookies session))
               (setf (alist-get (overleaf-project--cookie-domain) full-cookies nil nil #'string=)
                     (list (overleaf-project--webdriver-cookie-string cookies)
                           (overleaf-project--webdriver-cookie-expiry cookies)))
               (overleaf-project--save-and-apply-authenticated-cookies
                full-cookies
                "Saved Overleaf cookies for %s")
               full-cookies)))
       (overleaf-project--with-webdriver-direct-connection
         (when (oref (oref session service) process)
           (overleaf-project--async-unregister-process
            (oref (oref session service) process)))
         (overleaf-project--webdriver-session-stop session))))))

(defun overleaf-project--authenticate-1 (&optional url)
  "Synchronously authenticate to URL using `overleaf-project-auth-backend'."
  (pcase overleaf-project-auth-backend
    ('webdriver
     (overleaf-project--authenticate-with-webdriver url))
    ('firefox-cookies
     (overleaf-project--authenticate-with-firefox-cookies url))
    (_
     (user-error "Unsupported `overleaf-project-auth-backend': %S"
                 overleaf-project-auth-backend))))

;;;###autoload
(defun overleaf-project-authenticate (&optional url)
  "Obtain cookies for URL using `overleaf-project-auth-backend'.
If URL is nil, use `overleaf-project-url'."
  (interactive)
  (if (and (called-interactively-p 'interactive)
           (overleaf-project--async-enabled-p))
      (overleaf-project--start-authentication-async url)
    (overleaf-project--authenticate-1 url)))


(provide 'overleaf-project-auth)

;;; overleaf-project-auth.el ends here
