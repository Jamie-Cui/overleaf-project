;;; overleaf-project-auth.el --- Browser authentication for overleaf-project -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Webdriver-based Overleaf authentication.

;;; Code:

(require 'cl-lib)
(require 'webdriver)
(require 'webdriver-firefox)
(require 'overleaf-project-core)
(require 'overleaf-project-http)

;;;; Authentication

(defmacro overleaf--with-webdriver (&rest body)
  "Execute BODY if geckodriver is available."
  `(if (not (executable-find "geckodriver"))
       (message-box
        "Please install geckodriver to authenticate with Overleaf.")
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
            (goto-char (point-min))
            (when (re-search-forward
                   "Listening on .*:\\([0-9]+\\)\\(?:[[:space:]]*$\\|/\\)"
                   nil t)
              (setq port (string-to-number (match-string 1))))
            (unless port
              (accept-process-output process 0.1 nil t))))))
    (or port
        (and (integerp fallback) fallback)
        (error "Could not determine geckodriver listening port"))))

(cl-defmacro overleaf--webdriver-wait-until-appears
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

(defun overleaf--webdriver-cookie-string (cookies)
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

(defun overleaf--webdriver-project-url (href)
  "Return an absolute Overleaf project URL for HREF.
HREF may already be absolute or may be a relative path such as
\"/project/...\"."
  (and (stringp href)
       (url-expand-file-name href (concat (overleaf--url) "/"))))

(defun overleaf--webdriver-cookie-expiry (cookies)
  "Return the authenticated-session expiry for webdriver COOKIES, or nil.
Only cookie names matching `overleaf-auth-session-cookie-regexp' are
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
                     overleaf-auth-session-cookie-regexp
                     name))
          collect expiry)))
    (when expiries
      (apply #'min expiries))))

(defun overleaf-project--authenticate-1 (&optional url)
  "Synchronously use selenium webdriver to log into URL and obtain cookies.
If URL is nil, use `overleaf-url'.  Return the saved full cookie alist."
  (overleaf--with-webdriver
   (unless (and (boundp 'overleaf-save-cookies)
                overleaf-save-cookies)
     (user-error
      "`overleaf-save-cookies' needs to be configured"))
   (setq overleaf-url (or url (overleaf--url)))
   (let ((session (make-instance 'webdriver-session)))
     (unwind-protect
         ;; Re-authentication should not depend on previously saved cookies.
         ;; Using only the freshly captured cookie avoids failures from stale
         ;; or undecryptable cookie stores.
         (let ((full-cookies nil))
           (webdriver-session-start session)
           (webdriver-goto-url session (concat (overleaf--url) "/login"))
           (overleaf--message "Log in using the browser window...")
           (overleaf--webdriver-wait-until-appears
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
                        (overleaf--webdriver-project-url
                         (webdriver-get-element-attribute
                          session
                          first-project
                          "href"))))
                  (cookies nil))
             (when first-project-path
               (webdriver-goto-url session first-project-path))
             (setq cookies (webdriver-get-all-cookies session))
             (setf (alist-get (overleaf--cookie-domain) full-cookies nil nil #'string=)
                   (list (overleaf--webdriver-cookie-string cookies)
                         (overleaf--webdriver-cookie-expiry cookies)))
             (funcall overleaf-save-cookies
                      (prin1-to-string full-cookies))
             (overleaf-project--clear-csrf-cache)
             (setq overleaf--current-cookies
                   (overleaf--normalize-full-cookies full-cookies))
             (overleaf--message "Saved Overleaf cookies for %s"
                                (overleaf--cookie-domain))
             full-cookies))
       (webdriver-session-stop session)))))

;;;###autoload
(defun overleaf-project-authenticate (&optional url)
  "Use selenium webdriver to log into URL and obtain cookies.
If URL is nil, use `overleaf-url'."
  (interactive)
  (if (and (called-interactively-p 'interactive)
           (overleaf-project--async-command-enabled-p))
      (progn
        (unless (executable-find "geckodriver")
          (message-box
           "Please install geckodriver to authenticate with Overleaf.")
          (user-error "Required executable `geckodriver' was not found"))
        (unless (and (boundp 'overleaf-save-cookies)
                     overleaf-save-cookies)
          (user-error
           "`overleaf-save-cookies' needs to be configured"))
        (setq overleaf-url (or url (overleaf--url)))
        (overleaf-project--async-start
         (format "Overleaf authentication for %s" (overleaf--url-host))
         (lambda ()
           (overleaf-project--authenticate-1 overleaf-url))
         :key (format "auth:%s" (overleaf--url))
         :on-success
         (lambda (full-cookies)
           (overleaf-project--clear-csrf-cache)
           (setq overleaf--current-cookies
                 (overleaf--normalize-full-cookies full-cookies))
           (overleaf--message "Authentication finished for %s"
                              (overleaf--cookie-domain)))))
    (overleaf-project--authenticate-1 url)))


(provide 'overleaf-project-auth)

;;; overleaf-project-auth.el ends here
