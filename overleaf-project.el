;;; overleaf-project.el --- Clone, push, and pull full Overleaf projects with Git -*- lexical-binding: t; -*-
;; Copyright (C) 2020-2026 Jamie Cui

;; Author: Jamie Cui
;; Created: April 14, 2026
;; URL: https://github.com/Jamie-Cui/overleaf-project
;; Package-Requires: ((emacs "29.4") (plz "0.9") (websocket "1.15") (webdriver "0.1"))
;; Version: 2.0.0
;; Keywords: hypermedia, tex, tools
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides project-level Overleaf integration:
;;
;; - clone a full Overleaf project to a local Git repository
;; - push committed local changes to Overleaf and pull remote updates back
;; - detect remote divergence and open a dedicated Git merge branch
;;
;; Conflict resolution intentionally happens in Git, not ediff.  When
;; both local and remote changed, `overleaf-project-push' and
;; `overleaf-project-pull' create a sync branch, merge the downloaded
;; remote snapshot into it and leave conflicts to Magit or plain Git.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'json)
(require 'mm-url)
(require 'plz)
(require 'subr-x)
(require 'url-parse)
(require 'webdriver)
(require 'webdriver-firefox)
(require 'websocket)

(declare-function mm-url-decode-entities-string "mm-url")

;;;; Variables

(defgroup overleaf nil
  "Clone, push, and pull full Overleaf projects."
  :prefix "overleaf-"
  :group 'tools)

(defcustom overleaf-cookie-storage 'authinfo
  "Where Overleaf cookies should be persisted across Emacs sessions.

If the value is the symbol `authinfo', cookies are stored in the first
plain auth-source file from `auth-sources', falling back to
`~/.authinfo'.

If the value is a string, it is treated as a plain file path where the
serialized cookie alist is stored.

If the value is nil, authenticated cookies stay only in memory for the
current Emacs session."
  :type '(choice
          (const :tag "Emacs auth-source file" authinfo)
          (file :tag "Plain file")
          (const :tag "Session only" nil)))

(defcustom overleaf-cookies #'overleaf--load-configured-cookies
  "The Overleaf session cookies.

Can either be:

- an alist mapping domain keys to `(COOKIE-STRING EXPIRY)'
- a string containing either that serialized alist or a raw Cookie header
- a function returning one of those values

The default value follows `overleaf-cookie-storage'.  Override this
variable directly only when you want to supply cookies manually or use a
custom loader.

The cookies are usually obtained and refreshed via
`overleaf-project-authenticate'."
  :type '(choice sexp string function))


(defcustom overleaf-url "https://www.overleaf.com"
  "The URL of the Overleaf server."
  :type 'string)

(defcustom overleaf-cache-cookies t
  "Whether to cache the cookies after obtaining them."
  :type 'boolean)

(defcustom overleaf-debug nil
  "Whether to emit verbose debug messages."
  :type 'boolean)

(defcustom overleaf-git-executable "git"
  "Git executable used for project operations."
  :type 'string)

(defcustom overleaf-curl-executable "curl"
  "Curl executable used for project download and upload."
  :type 'string)

(defcustom overleaf-unzip-executable "unzip"
  "Unzip executable used to unpack downloaded projects."
  :type 'string)

(defcustom overleaf-project-base-ref "refs/overleaf/base"
  "Git ref that stores the last successfully synchronized snapshot."
  :type 'string)

(defcustom overleaf-project-sync-branch-prefix "overleaf-sync/"
  "Prefix for branches created during Overleaf merge conflict resolution."
  :type 'string)

(defcustom overleaf-project-socket-timeout 15
  "Seconds to wait for the Overleaf project tree websocket response."
  :type 'integer)

(defcustom overleaf-project-sync-auto-commit-message
  "chore: checkpoint before Overleaf push"
  "Commit message used when `overleaf-project-push' auto-commits changes."
  :type 'string)

(defcustom overleaf-auth-session-cookie-regexp
  "\\`\\(?:overleaf_session[[:alnum:]_]*\\|sharelatex_session[[:alnum:]_]*\\|sharelatex\\.sid\\|connect\\.sid\\|sessionid\\|session\\)\\'"
  "Regexp matching cookie names that represent the authenticated session.

This is used only for local expiry checks after `overleaf-project-authenticate'.
Short-lived analytics or load-balancer cookies are intentionally ignored,
because their expiry is often much earlier than the actual login session."
  :type 'regexp)

(defvar overleaf-save-cookies #'overleaf--save-configured-cookies
  "Function storing a freshly authenticated cookie string.

The function receives a string containing the session cookies and stores
them in a way that `overleaf-cookies' can later access.

The default implementation follows `overleaf-cookie-storage'.  Override
this variable directly only when you want custom persistence logic.")

(defvar overleaf--current-cookies nil
  "Cached cookie alist returned from `overleaf-cookies'.")

(defvar overleaf--csrf-cache (make-hash-table :test #'equal)
  "Cache of csrf tokens keyed by \"URL|PROJECT-ID\".")

(cl-defstruct overleaf-project--command-result
  "Result of an external command."
  status
  output)

(cl-defstruct overleaf-project--entity
  "A remote Overleaf project entity."
  path
  name
  id
  type
  parent-id)

(cl-defstruct overleaf-project--snapshot
  "A downloaded Overleaf project snapshot."
  temp-dir
  root)

(cl-defstruct overleaf-project--repo-status
  "Parsed `git status --porcelain' state."
  lines
  staged
  unstaged
  unmerged)

;;;; Logging

(defun overleaf--message (format-string &rest args)
  "Log an Overleaf message using FORMAT-STRING and ARGS."
  (apply #'message (concat "[overleaf] " format-string) args))

(defun overleaf--warn (format-string &rest args)
  "Display an Overleaf warning using FORMAT-STRING and ARGS."
  (display-warning
   'overleaf-project
   (apply #'format format-string args)
   :warning))

(defun overleaf--debug (format-string &rest args)
  "Log a debug message using FORMAT-STRING and ARGS."
  (when overleaf-debug
    (apply #'overleaf--message (concat "DEBUG: " format-string) args)))

;;;; Cookie helpers

(defconst overleaf--authinfo-default-source "~/.authinfo"
  "Fallback authinfo file used by Overleaf cookie helpers.")

(defconst overleaf--authinfo-default-user "overleaf-project"
  "Default authinfo login used for Overleaf cookie helpers.")

(defconst overleaf--authinfo-default-port "overleaf-cookie"
  "Default authinfo port used for Overleaf cookie helpers.")

(defconst overleaf--authinfo-record-marker "overleaf-cookie-record"
  "Marker key used for authinfo entries managed by this package.")

(defun overleaf--authinfo-source-file (&optional source)
  "Return the expanded authinfo SOURCE file path."
  (expand-file-name
   (or source
       (cl-some
        (lambda (entry)
          (and (stringp entry)
               (not (string-match-p "\\.gpg\\'" entry))
               entry))
        auth-sources)
       overleaf--authinfo-default-source)))

(defun overleaf--authinfo-resolve-host (&optional host)
  "Return the authinfo host key for HOST or the current Overleaf host."
  (downcase (or host (overleaf--url-host))))

(defun overleaf--authinfo-resolve-user (&optional user)
  "Return the authinfo login key for USER."
  (or user overleaf--authinfo-default-user))

(defun overleaf--authinfo-resolve-port (&optional port)
  "Return the authinfo port key for PORT."
  (or port overleaf--authinfo-default-port))

(defun overleaf--authinfo-format-value (value)
  "Return VALUE formatted as one authinfo token."
  (if (string-match-p "[[:space:]\"#]" value)
      (format "%S" value)
    value))

(defun overleaf--authinfo-format-field (name value)
  "Return one authinfo field string for NAME and VALUE."
  (format "%s %s" name (overleaf--authinfo-format-value value)))

(defun overleaf--authinfo-encode-secret (secret)
  "Encode serialized cookie SECRET for authinfo storage."
  (base64-encode-string secret t))

(defun overleaf--authinfo-decode-secret (secret)
  "Decode serialized cookie SECRET loaded from authinfo."
  (condition-case nil
      (base64-decode-string secret)
    (error secret)))

(defun overleaf--authinfo-entry-line (host user port secret)
  "Return one authinfo line storing SECRET for HOST, USER, and PORT."
  (string-join
   (list (overleaf--authinfo-format-field "machine" host)
         (overleaf--authinfo-format-field "login" user)
         (overleaf--authinfo-format-field "port" port)
         (overleaf--authinfo-format-field
          "password"
          (overleaf--authinfo-encode-secret secret))
         (overleaf--authinfo-format-field overleaf--authinfo-record-marker "t"))
   " "))

(defun overleaf--authinfo-entry-regexp (host user port)
  "Return a regexp matching a managed authinfo entry."
  (format "^machine %s login %s port %s password .* %s t$"
          (regexp-quote (overleaf--authinfo-format-value host))
          (regexp-quote (overleaf--authinfo-format-value user))
          (regexp-quote (overleaf--authinfo-format-value port))
          (regexp-quote overleaf--authinfo-record-marker)))

(defun overleaf--authinfo-read-secret (source host user port)
  "Read the Overleaf cookie secret from authinfo SOURCE."
  (let ((file (overleaf--authinfo-source-file source)))
    (when (file-readable-p file)
      (let* ((auth-sources (list file))
             (entry (car (auth-source-search
                          :max 1
                          :host (overleaf--authinfo-resolve-host host)
                          :user (overleaf--authinfo-resolve-user user)
                          :port (overleaf--authinfo-resolve-port port)
                          :require '(:secret)))))
        (when entry
          (overleaf--authinfo-decode-secret
           (auth-info-password entry)))))))

(defun overleaf--authinfo-write-secret (source host user port secret)
  "Write SECRET to authinfo SOURCE for HOST, USER, and PORT."
  (let* ((file (overleaf--authinfo-source-file source))
         (resolved-host (overleaf--authinfo-resolve-host host))
         (resolved-user (overleaf--authinfo-resolve-user user))
         (resolved-port (overleaf--authinfo-resolve-port port))
         (regexp (overleaf--authinfo-entry-regexp
                  resolved-host
                  resolved-user
                  resolved-port))
         (line (overleaf--authinfo-entry-line
                resolved-host
                resolved-user
                resolved-port
                secret))
         (existing
          (with-temp-buffer
            (when (file-readable-p file)
              (insert-file-contents file)
              (flush-lines regexp))
            (replace-regexp-in-string "\\`\n+" "" (buffer-string)))))
    (with-temp-file file
      (insert line)
      (unless (string-empty-p existing)
        (insert "\n" existing)))
    (set-file-modes file #o600)
    (auth-source-forget+ :host resolved-host :user resolved-user :port resolved-port)))

(defun overleaf--load-configured-cookies ()
  "Load cookies according to `overleaf-cookie-storage'."
  (pcase overleaf-cookie-storage
    ('authinfo
     (overleaf--authinfo-read-secret nil nil nil nil))
    ((pred stringp)
     (funcall (overleaf-project-read-cookies-from-file overleaf-cookie-storage)))
    (_ nil)))

(defun overleaf--save-configured-cookies (cookies)
  "Persist COOKIES according to `overleaf-cookie-storage'."
  (pcase overleaf-cookie-storage
    ('authinfo
     (overleaf--authinfo-write-secret nil nil nil nil cookies))
    ((pred stringp)
     (funcall (overleaf-project-save-cookies-to-file overleaf-cookie-storage) cookies))
    (_ nil)))

;;;###autoload
(defun overleaf-project-read-cookies-from-file (file)
  "Return a cookie loader function reading cookies from FILE.
To be used with `overleaf-cookies'."
  (lambda ()
    (with-temp-buffer
      (insert-file-contents (expand-file-name file))
      (read (string-trim (buffer-string))))))

(defun overleaf--normalize-cookie-entry (entry)
  "Normalize one cookie ENTRY into `(DOMAIN COOKIE-STRING EXPIRY)'."
  (pcase entry
    (`(,domain ,cookie-string ,expiry)
     (unless (and (stringp domain)
                  (stringp cookie-string)
                  (or (null expiry) (integerp expiry)))
       (error "Invalid Overleaf cookie entry: %S" entry))
     (list (downcase domain) cookie-string expiry))
    (`(,domain ,cookie-string)
     (unless (and (stringp domain) (stringp cookie-string))
       (error "Invalid Overleaf cookie entry: %S" entry))
     (list (downcase domain) cookie-string nil))
    (_
     (error "Invalid Overleaf cookie entry: %S" entry))))

(defun overleaf--normalize-full-cookies (cookies)
  "Return a normalized cookie alist from COOKIES."
  (cond
   ((null cookies) nil)
   ((stringp cookies)
    (let ((trimmed (string-trim cookies)))
      (cond
       ((string-empty-p trimmed) nil)
       ((string-prefix-p "(" trimmed)
        (condition-case err
            (overleaf--normalize-full-cookies
             (car (read-from-string trimmed)))
          (error
           (error "Could not parse serialized Overleaf cookies: %s"
                  (error-message-string err)))))
       (t
        (list (list (overleaf--cookie-domain) trimmed nil))))))
   ((listp cookies)
    (mapcar #'overleaf--normalize-cookie-entry cookies))
   (t
    (error "Unsupported value for `overleaf-cookies': %S" cookies))))

;;;###autoload
(defun overleaf-project-save-cookies-to-file (file)
  "Return a cookie saver function writing cookies to FILE.
To be used with `overleaf-save-cookies'."
  (lambda (cookies)
    (with-temp-file (expand-file-name file)
      (insert cookies))))

(defun overleaf--get-full-cookies ()
  "Load the association list mapping domains to cookies."
  (if (and overleaf--current-cookies overleaf-cache-cookies)
      overleaf--current-cookies
    (condition-case err
        (setq overleaf--current-cookies
              (overleaf--normalize-full-cookies
               (if (functionp overleaf-cookies)
                   (funcall overleaf-cookies)
                 overleaf-cookies)))
      (error
       (overleaf--warn "Error while loading cookies: %s"
                       (error-message-string err))
       nil))))

(defun overleaf--get-cookies ()
  "Load cookies from `overleaf-cookies'."
  (let ((state (overleaf--cookie-state)))
    (pcase (plist-get state :status)
      ('valid
       (plist-get state :value))
      ('expired
       (user-error
        "Cookies for %s are expired. Refresh them with `overleaf-project-authenticate' or manually"
        (overleaf--url-host)))
      (_
       (user-error
        "Cookies for %s are not set. Configure them with `overleaf-project-authenticate' or manually"
        (overleaf--url-host))))))

(defun overleaf--cookie-state ()
  "Return the local cookie state for the current `overleaf-url'.
The result is a plist with `:status' set to one of `valid',
`missing', or `expired'.  For `valid', `:value' contains the cookie
header string.  This only inspects locally available cookie data and
does not contact the Overleaf server."
  (let* ((entry
          (cl-some
           (lambda (domain)
             (alist-get
              domain
              (overleaf--get-full-cookies)
              nil
              nil
              #'string=))
           (overleaf--cookie-key-candidates)))
         (now (time-convert nil 'integer)))
    (if entry
        (pcase-let ((`(,value ,validity) entry))
          (if (or (not validity) (< now validity))
              `(:status valid :value ,value :validity ,validity)
            (setq overleaf--current-cookies nil)
            `(:status expired :validity ,validity)))
      (setq overleaf--current-cookies nil)
      '(:status missing))))

(defun overleaf--ensure-authenticated (&optional action)
  "Ensure the current `overleaf-url' has usable cookies before ACTION.
If cookies are missing or expired, ask in the minibuffer whether to run
`overleaf-project-authenticate' immediately."
  (let* ((host (overleaf--url-host))
         (state (overleaf--cookie-state))
         (status (plist-get state :status))
         (reason
          (pcase status
            ('expired
             (format
              "Cookies for %s are expired according to the locally saved expiry time."
              host))
            ('missing
             (format "Cookies for %s are not set locally." host))
            (_ nil))))
    (if (eq status 'valid)
        t
      (if (or noninteractive
              (not
               (let ((use-dialog-box nil))
                 (y-or-n-p
                  (format "%s Re-run `overleaf-project-authenticate` now? "
                          reason)))))
          (user-error
           "%s Run `overleaf-project-authenticate` before %s"
           reason
           (or action "continuing"))
        (overleaf-project-authenticate overleaf-url)
        (overleaf--get-cookies)
        t))))

;;;; Generic helpers

(defun overleaf--pget (plist &rest keys)
  "Recursively follow KEYS inside PLIST."
  (while keys
    (let ((key (pop keys)))
      (setq plist
            (if (integerp key)
                (nth key plist)
              (plist-get plist key)))))
  plist)

(defun overleaf--completing-read (prompt collection &optional padding)
  "Perform a completing read with PROMPT over COLLECTION.

COLLECTION is a list of plists with the shape
`(:fields (DISPLAY-FIELD...) :data DATA)'."
  (let* ((num-fields (1- (length (plist-get (car collection) :fields))))
         (padding (or padding 2))
         (field-widths
          (mapcar
           (lambda (field)
             (apply #'max
                    (mapcar
                     (lambda (row)
                       (length (nth field (plist-get row :fields))))
                     collection)))
           (number-sequence 0 num-fields)))
         (format-string
          (apply #'concat
                 (mapcar
                  (lambda (width)
                    (format "%%-%is" (+ padding width)))
                  field-widths)))
         (final-collection
          (cl-loop
           for row in collection
           collect
           `(,(apply #'format format-string (plist-get row :fields))
             ,(plist-get row :data)))))
    (cadr
     (assoc
      (completing-read prompt final-collection nil t)
      final-collection))))

(defun overleaf--url ()
  "Return a sanitized Overleaf URL without a trailing slash."
  (string-trim (string-trim overleaf-url) "" "/"))

(defun overleaf--url-host ()
  "Return the normalized host part of `overleaf-url'."
  (let ((host (url-host (url-generic-parse-url (overleaf--url)))))
    (unless host
      (user-error "Invalid Overleaf URL: %s" (overleaf--url)))
    (downcase host)))

(defun overleaf--cookie-domain ()
  "Return the cookie domain for the current `overleaf-url'."
  (overleaf--url-host))

(defun overleaf--cookie-key-candidates ()
  "Return candidate cookie keys for the current `overleaf-url'."
  (let* ((host (overleaf--url-host))
         (labels (string-split host "\\."))
         (candidates (list host (concat "." host))))
    ;; Keep parent-domain lookups for older saved cookie formats.
    (while (> (length labels) 2)
      (setq labels (cdr labels))
      (let ((suffix (string-join labels ".")))
        (setq candidates
              (append candidates (list suffix (concat "." suffix))))))
    (delete-dups candidates)))

(defun overleaf--project-page-url (project-id)
  "Return the project page URL for PROJECT-ID."
  (format "%s/project/%s" (overleaf--url) project-id))

(defun overleaf-project--sanitize-name (name)
  "Turn NAME into a filesystem-friendly directory name."
  (let ((sanitized
         (replace-regexp-in-string
          "-+"
          "-"
          (replace-regexp-in-string
           "[^[:alnum:]]+"
           "-"
           (downcase (string-trim name))))))
    (string-trim sanitized "-")))

(defun overleaf-project--ensure-executable (program)
  "Return PROGRAM if it is executable, otherwise signal an error."
  (or (executable-find program)
      (user-error "Required executable `%s' was not found" program)))

(defun overleaf-project--run (program args &optional directory env noerror)
  "Run PROGRAM with ARGS in DIRECTORY and optional ENV.
Return an `overleaf-project--command-result'.  Signal an error unless
NOERROR is non-nil."
  (let ((program (overleaf-project--ensure-executable program))
        (default-directory (or directory default-directory))
        (process-environment (append env process-environment)))
    (with-temp-buffer
      (let ((status (apply #'process-file program nil (current-buffer) nil args))
            (output nil))
        (setq output (string-trim-right (buffer-string)))
        (unless (or noerror (and (integerp status) (zerop status)))
          (error "%s %s failed: %s"
                 program
                 (string-join args " ")
                 (if (string-empty-p output) "unknown error" output)))
        (make-overleaf-project--command-result
         :status status
         :output output)))))

(defun overleaf-project--git-run (repo args &optional env noerror)
  "Run Git with ARGS in REPO and return a command result.
ENV is prepended to `process-environment'.  If NOERROR is non-nil, do
not signal on non-zero exit status."
  (overleaf-project--run overleaf-git-executable args repo env noerror))

(defun overleaf-project--git-output (repo &rest args)
  "Run Git with ARGS in REPO and return trimmed stdout."
  (overleaf-project--command-result-output
   (overleaf-project--git-run repo args)))

(defun overleaf-project--git-output-noerror (repo &rest args)
  "Run Git with ARGS in REPO and return stdout, or nil on failure."
  (let ((result (overleaf-project--git-run repo args nil t)))
    (when (and (integerp (overleaf-project--command-result-status result))
               (zerop (overleaf-project--command-result-status result)))
      (overleaf-project--command-result-output result))))

(defun overleaf-project-root (&optional directory)
  "Return the Git toplevel for DIRECTORY, or nil if none exists."
  (overleaf-project--git-output-noerror
   (or directory default-directory)
   "rev-parse" "--show-toplevel"))

(defun overleaf-project--require-repo (&optional directory)
  "Return the Git toplevel for DIRECTORY, or signal a user error."
  (or (and directory (overleaf-project-root directory))
      (overleaf-project-root default-directory)
      (user-error "Not inside a Git repository")))

(defun overleaf-project--require-managed-repo (&optional directory)
  "Return the managed Overleaf Git repo for DIRECTORY, or signal a user error."
  (let ((repo (overleaf-project--require-repo directory)))
    (unless (overleaf-project--managed-repo-p repo)
      (user-error "Repository %s is not configured as an Overleaf project" repo))
    repo))

(defun overleaf-project--set-repo-url (repo &optional url)
  "Set `overleaf-url' from REPO metadata or explicit URL, and return it."
  (setq overleaf-url
        (or url
            (and repo (overleaf-project--git-config-get repo "overleaf.url"))
            (overleaf--url))))

(defun overleaf-project--read-repo-status (repo)
  "Return parsed `git status --porcelain' information for REPO."
  (let* ((output (overleaf-project--git-output repo "status" "--porcelain"))
         (lines (unless (string-empty-p output)
                  (split-string output "\n" t)))
         (staged nil)
         (unstaged nil)
         (unmerged nil))
    (dolist (line lines)
      (let* ((code (substring line 0 2))
             (index-status (aref code 0))
             (worktree-status (aref code 1)))
        (when (member code '("DD" "AU" "UD" "UA" "DU" "AA" "UU"))
          (setq unmerged t))
        (when (and (not (eq index-status ?\s))
                   (not (eq index-status ?\?)))
          (setq staged t))
        (when (or (string= code "??")
                  (and (not (eq worktree-status ?\s))
                       (not (eq worktree-status ?\?))))
          (setq unstaged t))))
    (make-overleaf-project--repo-status
     :lines lines
     :staged staged
     :unstaged unstaged
     :unmerged unmerged)))

(defun overleaf-project--current-branch (repo)
  "Return the current branch name for REPO.
Signal an error on detached HEAD."
  (let ((branch
         (overleaf-project--git-output repo "branch" "--show-current")))
    (if (string-empty-p branch)
        (user-error "Detached HEAD is not supported for Overleaf push/pull")
      branch)))

(defun overleaf-project--rev-parse (repo revision)
  "Resolve REVISION inside REPO."
  (overleaf-project--git-output repo "rev-parse" revision))

(defun overleaf-project--rev-parse-noerror (repo revision)
  "Resolve REVISION inside REPO, returning nil if it does not exist."
  (overleaf-project--git-output-noerror repo "rev-parse" "--verify" revision))

(defun overleaf-project--tree-id (repo revision)
  "Return the tree object id for REVISION inside REPO."
  (overleaf-project--git-output repo "rev-parse" (format "%s^{tree}" revision)))

(defun overleaf-project--merge-in-progress-p (repo)
  "Return non-nil if REPO currently has a merge in progress."
  (not (null
        (overleaf-project--rev-parse-noerror repo "MERGE_HEAD"))))

(defun overleaf-project--is-ancestor-p (repo ancestor descendant)
  "Return non-nil if ANCESTOR is an ancestor of DESCENDANT in REPO."
  (let ((result
         (overleaf-project--git-run
          repo
          (list "merge-base" "--is-ancestor" ancestor descendant)
          nil
          t)))
    (and (integerp (overleaf-project--command-result-status result))
         (zerop (overleaf-project--command-result-status result)))))

(defun overleaf-project--path-depth (path)
  "Return the slash depth of PATH."
  (if (string-empty-p path)
      0
    (length (split-string path "/" t))))

(defun overleaf-project--parent-path (path)
  "Return the parent directory path for PATH relative to the repo root."
  (let ((dir (file-name-directory path)))
    (if dir
        (directory-file-name dir)
      "")))

(defun overleaf-project--directory-empty-p (dir)
  "Return non-nil if DIR is empty or does not exist."
  (or (not (file-exists-p dir))
      (null
       (cl-remove-if
        (lambda (name)
          (member name '("." "..")))
        (directory-files dir nil nil t)))))

(defun overleaf-project--copy-directory-contents (source destination)
  "Copy SOURCE directory contents into DESTINATION."
  (make-directory destination t)
  (dolist (entry (directory-files source t nil t))
    (unless (member (file-name-nondirectory entry) '("." ".."))
      (let ((target
             (expand-file-name (file-name-nondirectory entry) destination)))
        (if (file-directory-p entry)
            (copy-directory entry target t t t)
          (copy-file entry target t t t t))))))

(defun overleaf-project--normalize-extracted-root (directory)
  "Return the effective project root inside DIRECTORY."
  (let ((entries
         (cl-remove-if
          (lambda (path)
            (member (file-name-nondirectory path) '("." "..")))
          (directory-files directory t nil t))))
    (if (and (= (length entries) 1)
             (file-directory-p (car entries)))
        (car entries)
      directory)))

(defun overleaf-project--files-equal-p (left right)
  "Return non-nil if LEFT and RIGHT have byte-identical contents."
  (and (file-exists-p left)
       (file-exists-p right)
       (= (file-attribute-size (file-attributes left))
          (file-attribute-size (file-attributes right)))
       (zerop (call-process "cmp" nil nil nil "--silent" left right))))

;;;; Git metadata

(defun overleaf-project--git-config-get (repo key)
  "Read Git config KEY from REPO."
  (overleaf-project--git-output-noerror repo "config" "--local" "--get" key))

(defun overleaf-project--git-config-set (repo key value)
  "Set Git config KEY to VALUE in REPO."
  (overleaf-project--git-output repo "config" "--local" key value))

(defun overleaf-project--git-config-unset (repo key)
  "Unset Git config KEY in REPO."
  (overleaf-project--git-run
   repo
   (list "config" "--local" "--unset-all" key)
   nil
   t))

(defun overleaf-project--set-base-ref (repo revision)
  "Move the Overleaf base ref in REPO to REVISION."
  (overleaf-project--git-output
   repo
   "update-ref"
   (or (overleaf-project--git-config-get repo "overleaf.baseRef")
       overleaf-project-base-ref)
   revision))

(defun overleaf-project--base-ref (repo)
  "Return the configured base ref for REPO."
  (or (overleaf-project--git-config-get repo "overleaf.baseRef")
      overleaf-project-base-ref))

(defun overleaf-project--project-id (repo)
  "Return the configured Overleaf project id for REPO."
  (or (overleaf-project--git-config-get repo "overleaf.projectId")
      (user-error "Repository %s is not configured as an Overleaf project" repo)))

(defun overleaf-project--project-name (repo)
  "Return the configured Overleaf project name for REPO."
  (or (overleaf-project--git-config-get repo "overleaf.projectName")
      (overleaf-project--project-id repo)))

(defun overleaf-project--managed-repo-p (repo)
  "Return non-nil if REPO stores Overleaf project metadata."
  (not (null (overleaf-project--git-config-get repo "overleaf.projectId"))))

(defun overleaf-project--write-repo-metadata (repo project)
  "Persist PROJECT metadata inside REPO."
  (overleaf-project--git-config-set
   repo "overleaf.projectId" (plist-get project :id))
  (overleaf-project--git-config-set
   repo "overleaf.projectName" (plist-get project :name))
  (overleaf-project--git-config-set
   repo "overleaf.url" (overleaf--url))
  (overleaf-project--git-config-set
   repo "overleaf.baseRef" overleaf-project-base-ref))

(defun overleaf-project--clear-pending-state (repo)
  "Remove all pending push/pull metadata from REPO."
  (dolist (key '("overleaf.pendingOriginalBranch"
                 "overleaf.pendingOriginalHead"
                 "overleaf.pendingSyncBranch"
                 "overleaf.pendingRemoteCommit"
                 "overleaf.pendingAction"))
    (overleaf-project--git-config-unset repo key)))

(defun overleaf-project--set-pending-state
    (repo original-branch original-head sync-branch remote-commit action)
  "Persist a pending push/pull state inside REPO."
  (overleaf-project--git-config-set
   repo "overleaf.pendingOriginalBranch" original-branch)
  (overleaf-project--git-config-set
   repo "overleaf.pendingOriginalHead" original-head)
  (overleaf-project--git-config-set
   repo "overleaf.pendingSyncBranch" sync-branch)
  (overleaf-project--git-config-set
   repo "overleaf.pendingRemoteCommit" remote-commit)
  (overleaf-project--git-config-set
   repo "overleaf.pendingAction" (symbol-name action)))

(defun overleaf-project--pending-state (repo)
  "Return pending push/pull metadata for REPO, or nil."
  (when-let* ((sync-branch
               (overleaf-project--git-config-get
                repo "overleaf.pendingSyncBranch")))
    (let ((action
           (or (overleaf-project--git-config-get repo "overleaf.pendingAction")
               "push")))
      `(:sync-branch ,sync-branch
                     :action ,(intern action)
                     :original-branch
                     ,(overleaf-project--git-config-get repo "overleaf.pendingOriginalBranch")
                     :original-head
                     ,(overleaf-project--git-config-get repo "overleaf.pendingOriginalHead")
                     :remote-commit
                     ,(overleaf-project--git-config-get repo "overleaf.pendingRemoteCommit")))))

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
                (plz 'get
                  (overleaf--project-page-url project-id)
                  :headers
                  `(("Cookie" . ,cookies)
                    ("Origin" . ,(overleaf--url))
                    ("Referer" . ,(overleaf--project-page-url project-id)))))
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

(defun overleaf-project--socket-cookies ()
  "Return cookies suitable for websocket access."
  (let* ((cookies (overleaf--get-cookies))
         (response
          (plz 'get
            (format "%s/socket.io/socket.io.js" (overleaf--url))
            :as 'response
            :headers `(("Cookie" . ,cookies)
                       ("Origin" . ,(overleaf--url)))))
         (set-cookie (alist-get 'set-cookie (plz-response-headers response)))
         (gclb-cookie
          (and set-cookie
               (save-match-data
                 (when (string-match "\\(GCLB=.*?\\);" set-cookie)
                   (match-string 1 set-cookie))))))
    (if (and gclb-cookie (not (string-empty-p gclb-cookie)))
        (format "%s; %s" cookies gclb-cookie)
      cookies)))

(defun overleaf-project--curl-download-args (url output-file headers)
  "Return curl argument list to download URL into OUTPUT-FILE with HEADERS."
  (append
   '("--fail" "--silent" "--show-error" "--location")
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
          '("--fail" "--silent" "--show-error" "--location")
          (list "-X" method)
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
            '("--fail" "--silent" "--show-error" "--location" "-X" "POST")
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

(defun overleaf-project--with-downloaded-snapshot (project-id function)
  "Download PROJECT-ID, call FUNCTION with the snapshot root, then clean up."
  (let ((snapshot nil))
    (unwind-protect
        (progn
          (setq snapshot (overleaf-project--download-snapshot project-id))
          (funcall function (overleaf-project--snapshot-root snapshot)))
      (when snapshot
        (ignore-errors
          (delete-directory
           (overleaf-project--snapshot-temp-dir snapshot)
           t))))))

(defun overleaf-project--fetch-remote-table (project-id)
  "Return the remote entity table for PROJECT-ID."
  (overleaf-project--build-entity-table
   (overleaf-project--fetch-tree project-id)))

(defun overleaf-project--with-remote-state (project-id function)
  "Download PROJECT-ID and call FUNCTION with the remote root and entity table."
  (overleaf-project--with-downloaded-snapshot
   project-id
   (lambda (remote-root)
     (funcall function
              remote-root
              (overleaf-project--fetch-remote-table project-id)))))

(defun overleaf-project--create-folder (project-id parent-id name)
  "Create folder NAME below PARENT-ID on PROJECT-ID."
  (json-parse-string
   (plz 'post
     (format "%s/project/%s/folder" (overleaf--url) project-id)
     :headers (append
               (overleaf-project--project-headers project-id)
               '(("Content-Type" . "application/json")))
     :body (json-encode `(:parent_folder_id ,parent-id :name ,name)))
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
          (plz 'get
            (format "%s/project" (overleaf--url))
            :headers `(("Cookie" . ,cookies)
                       ("Origin" . ,(overleaf--url)))))
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

(defun overleaf-project--read-project (&optional url)
  "Prompt for an Overleaf project on URL and return its plist."
  (let* ((projects (overleaf-project-list url))
         (collection
          (mapcar
           (lambda (project)
             `(:fields
               (,(plist-get project :name)
                ,(or (plist-get (plist-get project :owner) :email) ""))
               :data ,project))
           projects)))
    (overleaf--completing-read "Project: " collection)))

;;;; Remote project tree

(defun overleaf-project--fetch-tree (project-id)
  "Return PROJECT-ID's root folder plist via websocket."
  (let* ((cookies (overleaf-project--socket-cookies))
         (response
          (plz 'get
            (format "%s/socket.io/1/?projectId=%s&esh=1&ssp=1"
                    (overleaf--url)
                    project-id)
            :as 'response
            :headers `(("Cookie" . ,cookies)
                       ("Origin" . ,(overleaf--url)))))
         (ws-id (car (string-split (plz-response-body response) ":")))
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
                 (if (file-directory-p entry)
                     (progn
                       (puthash relative entry dirs)
                       (walk entry))
                   (puthash relative entry files)))))))
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
    (overleaf--warn
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

(defun overleaf-project--prepare-working-tree-for-sync (repo)
  "Stage and commit local changes in REPO when needed for pushing."
  (let ((status (overleaf-project--read-repo-status repo)))
    (when (overleaf-project--repo-status-unmerged status)
      (user-error
       "Repository %s has unresolved merge conflicts; resolve them before pushing"
       repo))
    (when (overleaf-project--repo-status-unstaged status)
      (unless
          (y-or-n-p
           (format
            "Repository %s has unstaged changes. Stage all changes and continue with Overleaf push? "
            repo))
        (user-error
         "Overleaf push requires a clean working tree; stage all changes or stash them first"))
      (overleaf-project--git-output repo "add" "--all" ".")
      (setq status (overleaf-project--read-repo-status repo)))
    (when (overleaf-project--repo-status-staged status)
      (overleaf--message "Committing local changes before Overleaf push...")
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

(defun overleaf-project--create-sync-branch-name (repo)
  "Return a fresh sync branch name for REPO."
  (let* ((base
          (format "%s%s"
                  overleaf-project-sync-branch-prefix
                  (format-time-string "%Y%m%d-%H%M%S")))
         (candidate base)
         (suffix 1))
    (while (overleaf-project--rev-parse-noerror
            repo
            (format "refs/heads/%s" candidate))
      (setq candidate (format "%s-%d" base suffix))
      (setq suffix (1+ suffix)))
    candidate))

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
         (unless (or (gethash path local-files)
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

(defun overleaf-project--sync-commit
    (repo revision project-id remote-root remote-table)
  "Synchronize REVISION from REPO into PROJECT-ID."
  (let ((local-root nil))
    (unwind-protect
        (progn
          (setq local-root (overleaf-project--materialize-commit repo revision))
          (overleaf--message "Uploading %s to Overleaf..." revision)
          (overleaf-project--sync-local-tree
           project-id local-root remote-root remote-table))
      (when local-root
        (ignore-errors (delete-directory local-root t))))))

(defun overleaf-project--record-remote-snapshot (repo remote-root)
  "Create a Git commit in REPO representing REMOTE-ROOT."
  (overleaf-project--commit-directory
   repo
   remote-root
   (overleaf-project--rev-parse-noerror repo (overleaf-project--base-ref repo))
   (format "overleaf: remote snapshot %s"
           (format-time-string "%Y-%m-%d %H:%M:%S"))))

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
           (sync-branch (plist-get pending :sync-branch))
           (current-branch (overleaf-project--current-branch repo)))
      (unless (string= current-branch sync-branch)
        (user-error
         "Pending Overleaf %s exists on branch `%s'; checkout that branch and rerun %s"
         pending-action
         sync-branch
         pending-command))
      (unless (eq pending-action action)
        (user-error
         "Pending Overleaf %s exists on branch `%s'; rerun %s"
         pending-action
         sync-branch
         pending-command)))))

(defun overleaf-project--ensure-no-pending-action (repo command)
  "Signal if REPO still has a pending Overleaf sync before COMMAND."
  (when-let* ((pending (overleaf-project--pending-state repo)))
    (user-error
     "Pending Overleaf %s exists on branch `%s'; finish it before %s"
     (or (plist-get pending :action) 'push)
     (plist-get pending :sync-branch)
     command)))

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
and FORMAT-STRING with ARGS is passed to `overleaf--message'."
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
  (apply #'overleaf--message format-string args))

(defun overleaf-project--note-matching-sync-state (repo head)
  "Update REPO base metadata after confirming HEAD already matches Overleaf."
  (overleaf-project--set-base-ref repo head)
  (overleaf--message "Local and remote content already match; base ref updated"))

(defun overleaf-project--upload-head-and-set-base
    (repo head project-id remote-root remote-table format-string &rest args)
  "Upload HEAD from REPO to Overleaf, update the base ref, and report success."
  (overleaf-project--sync-commit
   repo head project-id remote-root remote-table)
  (overleaf-project--set-base-ref repo head)
  (apply #'overleaf--message format-string args))

(defun overleaf-project--start-pending-sync
    (repo branch head remote-commit action finalize)
  "Create a pending sync branch for ACTION and merge REMOTE-COMMIT.
FINALIZE is called with no arguments after a successful merge."
  (let ((sync-branch (overleaf-project--create-sync-branch-name repo)))
    (overleaf-project--git-output repo "branch" sync-branch head)
    (overleaf-project--git-output repo "checkout" sync-branch)
    (overleaf-project--set-pending-state
     repo branch head sync-branch remote-commit action)
    (let ((merge-result
           (overleaf-project--git-run
            repo
            (list "merge" "--no-ff" "--no-edit" remote-commit)
            nil
            t)))
      (if (and (integerp (overleaf-project--command-result-status merge-result))
               (zerop (overleaf-project--command-result-status merge-result)))
          (funcall finalize)
        (overleaf--warn
         "Overleaf %s needs manual conflict resolution on branch `%s'. Resolve the merge, commit it, then rerun `overleaf-project-%s`."
         action
         sync-branch
         action)))))

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

(defun overleaf-project--finalize-pending-pull (repo pending remote-root)
  "Finalize PENDING pull in REPO using REMOTE-ROOT."
  (let* ((context (overleaf-project--validate-pending-sync repo pending 'pull))
         (head (plist-get context :head))
         (original-head (plist-get context :original-head))
         (original-branch (plist-get context :original-branch))
         (remote-tree (plist-get context :remote-tree)))
    (overleaf-project--ensure-pending-remote-unchanged
     repo remote-root remote-tree 'pull)
    (overleaf-project--set-base-ref repo head)
    (overleaf-project--finish-pending-sync
     repo
     'pull
     original-branch
     original-head
     head
     "Pulled Overleaf changes into `%s' and updated branch `%s'"
     (overleaf-project--project-name repo)
     original-branch)))

(defun overleaf-project--fresh-push (repo remote-root remote-table)
  "Perform a fresh push of REPO using REMOTE-ROOT and REMOTE-TABLE."
  (let* ((context (overleaf-project--read-sync-state repo remote-root))
         (head (plist-get context :head))
         (branch (plist-get context :branch))
         (project-id (overleaf-project--project-id repo))
         (remote-commit (plist-get context :remote-commit))
         (status (plist-get context :status)))
    (pcase status
      ('in-sync
       (overleaf--message "Project `%s' is already in sync"
                          (overleaf-project--project-name repo)))
      ('head-matches-remote
       (overleaf-project--note-matching-sync-state repo head))
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
       (overleaf-project--start-pending-sync
        repo
        branch
        head
        remote-commit
        'push
        (lambda ()
          (overleaf-project--finalize-pending-push
           repo
           (overleaf-project--pending-state repo)
           remote-root
           remote-table)))))))

(defun overleaf-project--fresh-pull (repo remote-root)
  "Perform a fresh pull of REPO using REMOTE-ROOT."
  (let* ((context (overleaf-project--read-sync-state repo remote-root))
         (head (plist-get context :head))
         (branch (plist-get context :branch))
         (remote-commit (plist-get context :remote-commit))
         (status (plist-get context :status)))
    (pcase status
      ('in-sync
       (overleaf--message "Project `%s' is already in sync"
                          (overleaf-project--project-name repo)))
      ('head-matches-remote
       (overleaf-project--note-matching-sync-state repo head))
      ('remote-matches-base
       (overleaf--message "No remote Overleaf changes to pull into `%s'" branch))
      ('head-matches-base
       (overleaf-project--git-output repo "merge" "--ff-only" remote-commit)
       (overleaf-project--set-base-ref repo "HEAD")
       (overleaf--message "Pulled remote Overleaf changes into `%s'" branch))
      (_
       (overleaf-project--start-pending-sync
        repo
        branch
        head
        remote-commit
        'pull
        (lambda ()
          (overleaf-project--finalize-pending-pull
           repo
           (overleaf-project--pending-state repo)
           remote-root)))))))

;;;; Interactive commands

;;;###autoload
(defun overleaf-project-clone (&optional url target-directory)
  "Clone a full Overleaf project into TARGET-DIRECTORY.
If URL is nil, use `overleaf-url'."
  (interactive)
  (let* ((url (or url (overleaf--url)))
         (project nil)
         (target nil)
         (repo nil))
    (setq overleaf-url url)
    (overleaf--ensure-authenticated "cloning from Overleaf")
    (setq project (overleaf-project--read-project url))
    (setq target
          (or target-directory
              (read-file-name
               "Clone to directory: "
               default-directory
               (expand-file-name
                (overleaf-project--sanitize-name (plist-get project :name))
                default-directory)
               nil
               (overleaf-project--sanitize-name (plist-get project :name)))))
    (setq target (directory-file-name (expand-file-name target)))
    (when (and (file-exists-p target)
               (not (file-directory-p target)))
      (user-error "Target path %s exists and is not a directory" target))
    (unless (overleaf-project--directory-empty-p target)
      (user-error "Target directory %s is not empty" target))
    (overleaf-project--with-downloaded-snapshot
     (plist-get project :id)
     (lambda (snapshot-root)
       (make-directory target t)
       (overleaf-project--copy-directory-contents snapshot-root target)
       (setq repo target)
       (overleaf-project--git-output repo "init")
       (overleaf-project--write-repo-metadata repo project)
       (overleaf-project--git-output repo "add" "--all" ".")
       (apply
        #'overleaf-project--git-output
        repo
        (append
         (overleaf-project--git-identity-args repo)
         '("commit" "-m" "chore: import project from Overleaf")))
       (overleaf-project--set-base-ref repo "HEAD")
       (overleaf--message
        "Cloned `%s' into %s"
        (plist-get project :name)
        target)))))

;;;###autoload
(defun overleaf-project-init (&optional directory url)
  "Bind the Git repo in DIRECTORY to a remote Overleaf project on URL.
The command stores project metadata and initializes the hidden base
snapshot used by later `overleaf-project-push' and
`overleaf-project-pull' runs, but does not automatically pull or push."
  (interactive)
  (let* ((repo (overleaf-project--require-repo directory))
         (current-id nil)
         (current-name nil)
         (project nil))
    (overleaf-project--ensure-no-pending-action repo "reconfiguring the repository")
    (overleaf-project--set-repo-url repo url)
    (overleaf--ensure-authenticated "configuring the Overleaf project")
    (setq current-id (overleaf-project--git-config-get repo "overleaf.projectId"))
    (setq current-name (overleaf-project--git-config-get repo "overleaf.projectName"))
    (setq project (overleaf-project--read-project overleaf-url))
    (when (and (called-interactively-p 'interactive)
               current-id
               (not (yes-or-no-p
                     (if (string= current-id (plist-get project :id))
                         (format
                          "Reinitialize the Overleaf base snapshot for `%s` against `%s'? "
                          repo
                          (or current-name current-id))
                       (format
                        "Rebind `%s' from Overleaf project `%s' to `%s'? "
                        repo
                        (or current-name current-id)
                        (plist-get project :name))))))
      (user-error "Aborted"))
    (overleaf-project--with-downloaded-snapshot
     (plist-get project :id)
     (lambda (snapshot-root)
       (overleaf-project--initialize-base-ref repo project snapshot-root)
       (overleaf--message
        "Configured `%s' to track Overleaf project `%s' without pulling or pushing"
        repo
        (plist-get project :name))))))

;;;###autoload
(defun overleaf-project-push (&optional directory noerror)
  "Push the current Git repo to its configured Overleaf project.
Staged changes are committed automatically before the remote snapshot is
fetched.  When unstaged changes exist, prompt whether to stage them
first.

When NOERROR is non-nil, silently return nil if DIRECTORY is not a
managed Overleaf repo, and demote push errors to warnings.  This is
useful for hooks such as `git-commit-post-finish-hook'."
  (interactive)
  (let* ((repo (or (and directory (overleaf-project-root directory))
                   (overleaf-project-root default-directory))))
    (cond
     ((not (and repo (overleaf-project--managed-repo-p repo)))
      (if noerror
          nil
        (user-error "Repository %s is not configured as an Overleaf project"
                     (or repo default-directory))))
     (noerror
      (condition-case err
          (overleaf-project--push-1 repo)
        (error
         (overleaf--warn "Automatic Overleaf push failed for %s: %s"
                         repo (error-message-string err)))))
     (t
      (overleaf-project--push-1 repo)))))

(defun overleaf-project--push-1 (repo)
  "Internal: perform the actual push for managed REPO."
  (let ((pending nil)
        (project-id nil))
    (overleaf-project--set-repo-url repo)
    (setq pending (overleaf-project--pending-state repo))
    (overleaf-project--ensure-pending-action repo pending 'push)
    (overleaf--ensure-authenticated "pushing to Overleaf")
    (if pending
        (overleaf-project--ensure-clean-working-tree repo "finishing the pending Overleaf push")
      (overleaf-project--prepare-working-tree-for-sync repo))
    (setq project-id (overleaf-project--project-id repo))
    (overleaf-project--with-remote-state
     project-id
     (lambda (remote-root remote-table)
       (if pending
           (overleaf-project--finalize-pending-push
            repo pending remote-root remote-table)
         (overleaf-project--fresh-push repo remote-root remote-table))))))

;;;###autoload
(defun overleaf-project-push-force (&optional directory)
  "Force push the current Git repo to its configured Overleaf project.
Like `overleaf-project-push', staged changes are committed automatically
before upload.  Unlike `overleaf-project-push', remote Overleaf changes
are overwritten by the local `HEAD' snapshot."
  (interactive)
  (let* ((repo (overleaf-project--require-managed-repo directory))
         (project-id nil)
         (context nil))
    (overleaf-project--ensure-no-pending-action repo "force pushing")
    (overleaf-project--set-repo-url repo)
    (setq project-id (overleaf-project--project-id repo))
    (overleaf--ensure-authenticated "force pushing to Overleaf")
    (when (and (called-interactively-p 'interactive)
               (not
                (yes-or-no-p
                 (format
                  "Force push local HEAD to Overleaf project `%s'? This will overwrite remote files. "
                  (overleaf-project--project-name repo)))))
      (user-error "Aborted"))
    (overleaf-project--prepare-working-tree-for-sync repo)
    (overleaf-project--with-remote-state
     project-id
     (lambda (remote-root remote-table)
       (setq context (overleaf-project--read-sync-state repo remote-root))
       (if (memq (plist-get context :status) '(in-sync head-matches-remote))
           (overleaf-project--note-matching-sync-state
            repo
            (plist-get context :head))
         (overleaf-project--upload-head-and-set-base
          repo
          (plist-get context :head)
          project-id
          remote-root
          remote-table
          "Force pushed `%s' to Overleaf"
          (overleaf-project--project-name repo)))))))

;;;###autoload
(defun overleaf-project-pull (&optional directory)
  "Pull the latest Overleaf snapshot into the current Git repo.
The working tree must be clean before pulling."
  (interactive)
  (let* ((repo (overleaf-project--require-managed-repo directory))
         (pending nil))
    (overleaf-project--set-repo-url repo)
    (setq pending (overleaf-project--pending-state repo))
    (overleaf-project--ensure-pending-action repo pending 'pull)
    (overleaf-project--ensure-clean-working-tree repo "pulling from Overleaf")
    (overleaf--ensure-authenticated "pulling from Overleaf")
    (overleaf-project--with-downloaded-snapshot
     (overleaf-project--project-id repo)
     (lambda (remote-root)
       (if pending
           (overleaf-project--finalize-pending-pull repo pending remote-root)
         (overleaf-project--fresh-pull repo remote-root))))))



;;;###autoload
(defun overleaf-project-browse-remote (&optional directory)
  "Open the configured Overleaf project in a browser."
  (interactive)
  (let* ((repo (or (and directory (overleaf-project-root directory))
                   (overleaf-project-root default-directory)))
         (project-id
          (if repo
              (overleaf-project--project-id repo)
            (plist-get (overleaf-project--read-project) :id))))
    (when repo
      (overleaf-project--set-repo-url repo))
    (browse-url (overleaf--project-page-url project-id))))

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

;;;###autoload
(defun overleaf-project-authenticate (&optional url)
  "Use selenium webdriver to log into URL and obtain cookies.
If URL is nil, use `overleaf-url'."
  (interactive)
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
                                (overleaf--cookie-domain))))
       (webdriver-session-stop session)))))

;;;; Command map

;;;###autoload
(defvar-keymap overleaf-project-command-map
  "a" #'overleaf-project-authenticate
  "b" #'overleaf-project-browse-remote
  "c" #'overleaf-project-clone
  "l" #'overleaf-project-pull
  "p" #'overleaf-project-push
  "s" #'overleaf-project-push)

(provide 'overleaf-project)

;;; overleaf-project.el ends here
