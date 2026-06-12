;;; overleaf-project-firefox.el --- Firefox cookie import for overleaf-project -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Assisted-by: Codex:GPT-5.5
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Imports Overleaf cookies from an existing Firefox profile.

;;; Code:

(require 'cl-lib)
(require 'overleaf-project-core)
(require 'sqlite)
(require 'subr-x)

;;;; Customization

(defcustom overleaf-project-firefox-profile nil
  "Firefox profile directory used for importing Overleaf cookies.

When nil, `overleaf-project-authenticate' with the `firefox-cookies'
backend reads Firefox's profiles.ini and uses its default profile."
  :type '(choice
          (directory :tag "Firefox profile directory")
          (const :tag "Auto-detect default profile" nil))
  :group 'overleaf-project)

;;;; Profile discovery

(defun overleaf-project-firefox--profiles-ini-candidates ()
  "Return possible Firefox profiles.ini paths for this system."
  (delq
   nil
   (list
    (expand-file-name "~/Library/Application Support/Firefox/profiles.ini")
    (expand-file-name "~/.mozilla/firefox/profiles.ini")
    (when-let* ((appdata (getenv "APPDATA")))
      (expand-file-name "Mozilla/Firefox/profiles.ini" appdata)))))

(defun overleaf-project-firefox--profiles-ini-file ()
  "Return the readable Firefox profiles.ini file, or nil."
  (cl-find-if #'file-readable-p
              (overleaf-project-firefox--profiles-ini-candidates)))

(defun overleaf-project-firefox--parse-profiles-ini (file)
  "Parse Firefox profiles.ini FILE and return section plists."
  (let ((sections nil)
        (current nil))
    (cl-labels
        ((finish-section ()
           (when current
             (push current sections)
             (setq current nil)))
         (plist-key (key)
           (intern (concat ":" (downcase key)))))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (string-trim
                       (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))))
            (cond
             ((or (string-empty-p line)
                  (string-prefix-p ";" line)
                  (string-prefix-p "#" line)))
             ((string-match "\\`\\[\\([^]]+\\)\\]\\'" line)
              (finish-section)
              (setq current (list :section (match-string 1 line))))
             ((and current
                   (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" line))
              (setf (plist-get current
                               (plist-key (string-trim (match-string 1 line))))
                    (string-trim (match-string 2 line))))))
          (forward-line 1)))
      (finish-section))
    (nreverse sections)))

(defun overleaf-project-firefox--profile-section-p (section)
  "Return non-nil if SECTION describes a Firefox profile."
  (string-prefix-p "Profile" (or (plist-get section :section) "")))

(defun overleaf-project-firefox--install-section-p (section)
  "Return non-nil if SECTION describes a Firefox install."
  (string-prefix-p "Install" (or (plist-get section :section) "")))

(defun overleaf-project-firefox--install-default-section (sections)
  "Return a profile section derived from a Firefox install default in SECTIONS.
Firefox's dedicated-profile feature records the active profile in an
\"[InstallXXX]\" section.  Its `Default' entry is a profile path,
relative to the profiles.ini directory."
  (when-let* ((install
               (cl-find-if
                (lambda (section)
                  (and (overleaf-project-firefox--install-section-p section)
                       (plist-get section :default)))
                sections)))
    (list :section (plist-get install :section)
          :path (plist-get install :default)
          :isrelative "1")))

(defun overleaf-project-firefox--legacy-default-section (sections)
  "Return the legacy top-level `Default=1' Firefox profile in SECTIONS."
  (cl-find-if
   (lambda (section)
     (and (overleaf-project-firefox--profile-section-p section)
          (string= (plist-get section :default) "1")))
   sections))

(defun overleaf-project-firefox--default-profile-section (sections)
  "Return the default Firefox profile section from SECTIONS.

Modern Firefox keeps a dedicated profile per install and records it in
an \"[InstallXXX]\" section, which it treats as authoritative.  Prefer
that profile over the legacy top-level `Default=1' profile, which can
point to a stale, empty profile that no longer holds the user's
cookies."
  (or (overleaf-project-firefox--install-default-section sections)
      (overleaf-project-firefox--legacy-default-section sections)))

(defun overleaf-project-firefox--resolve-profile-path (section base-directory)
  "Return the absolute Firefox profile path for SECTION under BASE-DIRECTORY."
  (let ((path (plist-get section :path)))
    (unless (and (stringp path) (not (string-empty-p path)))
      (user-error "Firefox profiles.ini default profile has no Path entry"))
    (if (string= (plist-get section :isrelative) "1")
        (expand-file-name path base-directory)
      (expand-file-name path))))

(defun overleaf-project-firefox--profile-directory ()
  "Return the Firefox profile directory used for cookie import."
  (let ((profile
         (if overleaf-project-firefox-profile
             (expand-file-name overleaf-project-firefox-profile)
           (let* ((ini-file
                   (or (overleaf-project-firefox--profiles-ini-file)
                       (user-error
                        "Could not find Firefox profiles.ini.  Set `overleaf-project-firefox-profile' to a Firefox profile directory")))
                  (sections
                   (overleaf-project-firefox--parse-profiles-ini ini-file))
                  (section
                   (or (overleaf-project-firefox--default-profile-section sections)
                       (user-error
                        "Could not determine the default Firefox profile.  Set `overleaf-project-firefox-profile' manually"))))
             (overleaf-project-firefox--resolve-profile-path
              section
              (file-name-directory ini-file))))))
    (unless (file-directory-p profile)
      (user-error "Firefox profile directory does not exist: %s" profile))
    profile))

;;;; Cookie import

(defun overleaf-project-firefox--ensure-sqlite ()
  "Signal unless Emacs can open SQLite databases."
  (unless (and (fboundp 'sqlite-available-p)
               (sqlite-available-p))
    (user-error
     "`overleaf-project-auth-backend' is `firefox-cookies', but this Emacs was built without SQLite support")))

(defun overleaf-project-firefox--copy-cookie-store (profile)
  "Copy Firefox cookie SQLite files from PROFILE and return (DIR DB-FILE)."
  (let* ((source (expand-file-name "cookies.sqlite" profile))
         (temp-dir (make-temp-file "overleaf-firefox-cookies." t))
         (target (expand-file-name "cookies.sqlite" temp-dir)))
    (unless (file-readable-p source)
      (delete-directory temp-dir t)
      (user-error "Firefox profile %s does not contain a readable cookies.sqlite file" profile))
    (copy-file source target t)
    (dolist (suffix '("-wal" "-shm"))
      (let ((sidecar (concat source suffix)))
        (when (file-readable-p sidecar)
          (copy-file sidecar (concat target suffix) t))))
    (list temp-dir target)))

(defun overleaf-project-firefox--cookie-query (hosts)
  "Return a Firefox cookie SQL query matching HOSTS."
  (format
   "select name, value, host, path, expiry from moz_cookies where host in (%s)"
   (string-join (make-list (length hosts) "?") ", ")))

(defun overleaf-project-firefox--cookie-rows (db)
  "Return Firefox cookie rows from DB matching the current Overleaf host."
  (let ((hosts (overleaf-project--cookie-key-candidates)))
    (sqlite-select
     db
     (overleaf-project-firefox--cookie-query hosts)
     hosts)))

(defun overleaf-project-firefox--cookie-expired-p (row now)
  "Return non-nil if Firefox cookie ROW is expired at NOW."
  (let ((expiry (nth 4 row)))
    (and (integerp expiry)
         (> expiry 0)
         (<= expiry now))))

(defun overleaf-project-firefox--session-cookie-p (row)
  "Return non-nil if Firefox cookie ROW is an Overleaf session cookie."
  (let ((name (nth 0 row)))
    (and (stringp name)
         (string-match-p overleaf-project-auth-session-cookie-regexp name))))

(defun overleaf-project-firefox--cookie-header (rows)
  "Return an HTTP Cookie header string from Firefox cookie ROWS."
  (let ((pairs
         (cl-loop
          for row in rows
          for name = (nth 0 row)
          for value = (nth 1 row)
          when (and (stringp name)
                    (not (string-empty-p name))
                    (stringp value))
          collect (format "%s=%s" name value))))
    (unless pairs
      (user-error "No usable Overleaf cookies found in Firefox profile"))
    (string-join pairs "; ")))

(defun overleaf-project-firefox--session-expiry (rows)
  "Return the earliest positive session-cookie expiry in ROWS."
  (let ((expiries
         (cl-loop
          for row in rows
          for expiry = (nth 4 row)
          when (and (overleaf-project-firefox--session-cookie-p row)
                    (integerp expiry)
                    (> expiry 0))
          collect expiry)))
    (when expiries
      (apply #'min expiries))))

(defun overleaf-project-firefox--format-time (seconds)
  "Return a user-facing timestamp for SECONDS since the epoch."
  (format-time-string "%Y-%m-%d %H:%M:%S %Z" (seconds-to-time seconds)))

(defun overleaf-project-firefox--full-cookies-from-rows (rows profile)
  "Return normalized Overleaf full cookies from Firefox ROWS in PROFILE."
  (let* ((now (time-convert nil 'integer))
         (session-rows
          (cl-remove-if-not #'overleaf-project-firefox--session-cookie-p rows))
         (valid-rows
          (cl-remove-if
           (lambda (row)
             (overleaf-project-firefox--cookie-expired-p row now))
           rows))
         (valid-session-rows
          (cl-remove-if
           (lambda (row)
             (overleaf-project-firefox--cookie-expired-p row now))
           session-rows)))
    (cond
     ((null rows)
      (user-error
       "No Overleaf cookies found in Firefox profile %s.  Log in to %s in Firefox, then run `overleaf-project-authenticate' again"
       profile
       (overleaf-project--url)))
     ((null session-rows)
      (user-error
       "Found Overleaf cookies in Firefox profile %s, but no authenticated session cookie.  Log in to %s in Firefox, then run `overleaf-project-authenticate' again"
       profile
       (overleaf-project--url)))
     ((null valid-session-rows)
      (let ((expiry (overleaf-project-firefox--session-expiry session-rows)))
        (user-error
         "Firefox Overleaf session cookies in profile %s are expired%s.  Log in to %s in Firefox, then run `overleaf-project-authenticate' again"
         profile
         (if expiry
             (format " since %s"
                     (overleaf-project-firefox--format-time expiry))
           "")
         (overleaf-project--url))))
     ((null valid-rows)
      (user-error
       "Firefox Overleaf cookies in profile %s are expired.  Log in to %s in Firefox, then run `overleaf-project-authenticate' again"
       profile
       (overleaf-project--url)))
     (t
      (let ((expiry (overleaf-project-firefox--session-expiry
                     valid-session-rows)))
        (list
         (list (overleaf-project--cookie-domain)
               (overleaf-project-firefox--cookie-header valid-rows)
               expiry)))))))

;;;###autoload
(defun overleaf-project-firefox-cookies (&optional url)
  "Return Overleaf cookies imported from an existing Firefox profile.
If URL is nil, use `overleaf-project-url'.  The Firefox profile must
already contain a valid Overleaf login session."
  (let ((overleaf-project-url (or url (overleaf-project--url)))
        (profile nil)
        (copy nil)
        (db nil))
    (overleaf-project-firefox--ensure-sqlite)
    (setq profile (overleaf-project-firefox--profile-directory))
    (unwind-protect
        (progn
          (setq copy (overleaf-project-firefox--copy-cookie-store profile))
          (setq db (sqlite-open (cadr copy) t))
          (unless db
            (user-error "Could not open Firefox cookies database copied from %s" profile))
          (overleaf-project-firefox--full-cookies-from-rows
           (overleaf-project-firefox--cookie-rows db)
           profile))
      (when db
        (ignore-errors (sqlite-close db)))
      (when copy
        (ignore-errors (delete-directory (car copy) t))))))

(provide 'overleaf-project-firefox)

;;; overleaf-project-firefox.el ends here
