;;; overleaf-project-core.el --- Core helpers for overleaf-project -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Core customization, logging, cookie, Git, and filesystem helpers.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)

(declare-function overleaf-project-authenticate "overleaf-project-auth")

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

(defcustom overleaf-curl-connect-timeout 10
  "Seconds to wait while establishing Overleaf curl connections.
Set this to nil to let curl use its default connection timeout."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Use curl default" nil)))

(defcustom overleaf-curl-max-time 45
  "Maximum seconds to allow one Overleaf curl request to run.
Set this to nil to let curl run without a package-level request timeout."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "No package timeout" nil)))

(defcustom overleaf-unzip-executable "unzip"
  "Unzip executable used to unpack downloaded projects."
  :type 'string)

(defcustom overleaf-project-base-ref "refs/overleaf/base"
  "Git ref that stores the last successfully synchronized snapshot."
  :type 'string)

(defcustom overleaf-project-sync-metadata-enabled t
  "Whether to maintain a remote Overleaf sync metadata file.

When enabled, push commands upload `overleaf-project-sync-metadata-file'
to the Overleaf project root.  Downloaded snapshots read and remove that
file before Git comparisons, so it acts as remote bookkeeping rather
than project content."
  :type 'boolean)

(defcustom overleaf-project-sync-metadata-file ".overleaf-project-sync.json"
  "Root-level Overleaf file used to remember the last uploaded Git commit.

This file name is reserved by `overleaf-project' when
`overleaf-project-sync-metadata-enabled' is non-nil.  It is stored on the
Overleaf remote, but should not be tracked by the local Git repository."
  :type 'string)

(defcustom overleaf-project-local-backups-enabled t
  "Whether to create local backup refs before mutating Overleaf sync steps.

When enabled, operations that may move the current branch or complete a
pending sync first create refs under
`overleaf-project-local-backup-ref-prefix'.  These refs keep the previous
local commits reachable even if a later merge, fast-forward, or branch
update does not produce the expected result."
  :type 'boolean)

(defcustom overleaf-project-local-backup-ref-prefix "refs/overleaf/backups"
  "Git ref namespace used for local Overleaf safety backups."
  :type 'string)

(defcustom overleaf-project-socket-timeout 15
  "Seconds to wait for the Overleaf project tree websocket response."
  :type 'integer)

(defcustom overleaf-project-sync-auto-commit-message
  "chore: checkpoint before Overleaf push"
  "Commit message used when `overleaf-project-push' auto-commits changes."
  :type 'string)

(defcustom overleaf-project-async-commands t
  "Whether interactive Overleaf commands run long operations in the background.

When non-nil, commands such as `overleaf-project-clone',
`overleaf-project-push', and `overleaf-project-pull' collect necessary
user input in the foreground, then run network, unzip, and Git work on a
background Emacs thread.  Lisp callers that invoke these commands
noninteractively keep the synchronous behavior unless documented
otherwise."
  :type 'boolean)

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

(defvar overleaf-project--remote-sync-metadata nil
  "Sync metadata extracted from the currently downloaded remote snapshot.")

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

(cl-defstruct overleaf-project--async-completion
  "Completed background Overleaf operation."
  name
  key
  status
  value
  error
  on-success
  on-error
  default-directory
  overleaf-url)

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

;;;; Async helpers

(defvar overleaf-project--async-locks (make-hash-table :test #'equal)
  "Background operation locks keyed by repository or task identity.")

(defvar overleaf-project--async-completions nil
  "Completed background operations waiting for foreground callbacks.")

(defvar overleaf-project--async-mutex
  (and (fboundp 'make-mutex)
       (make-mutex "overleaf-project-async"))
  "Mutex protecting `overleaf-project--async-completions'.")

(defvar overleaf-project--async-timer nil
  "Timer that drains completed background operations.")

(defmacro overleaf-project--with-async-mutex (&rest body)
  "Run BODY while holding the async completion mutex."
  (declare (indent 0) (debug t))
  `(if overleaf-project--async-mutex
       (progn
         (mutex-lock overleaf-project--async-mutex)
         (unwind-protect
             (progn ,@body)
           (mutex-unlock overleaf-project--async-mutex)))
     ,@body))

(defun overleaf-project--async-supported-p ()
  "Return non-nil if background Emacs threads are available."
  (fboundp 'make-thread))

(defun overleaf-project--async-command-enabled-p ()
  "Return non-nil when foreground commands should start background work."
  (and overleaf-project-async-commands
       (not noninteractive)
       (overleaf-project--async-supported-p)))

(defun overleaf-project--async-lock-empty-p ()
  "Return non-nil if there are no active async locks."
  (let ((empty t))
    (maphash (lambda (_key _value)
               (setq empty nil))
             overleaf-project--async-locks)
    empty))

(defun overleaf-project--async-ensure-timer ()
  "Ensure the async completion drain timer is running."
  (unless (timerp overleaf-project--async-timer)
    (setq overleaf-project--async-timer
          (run-at-time 0.1 0.1 #'overleaf-project--async-drain-completions))))

(defun overleaf-project--async-stop-timer-if-idle ()
  "Stop the async completion timer when no background work remains."
  (when (and (timerp overleaf-project--async-timer)
             (null overleaf-project--async-completions)
             (overleaf-project--async-lock-empty-p))
    (cancel-timer overleaf-project--async-timer)
    (setq overleaf-project--async-timer nil)))

(defun overleaf-project--async-push-completion (completion)
  "Queue COMPLETION for foreground handling."
  (overleaf-project--with-async-mutex
    (push completion overleaf-project--async-completions)))

(defun overleaf-project--async-pop-completions ()
  "Return queued async completions in completion order."
  (overleaf-project--with-async-mutex
    (prog1 (nreverse overleaf-project--async-completions)
      (setq overleaf-project--async-completions nil))))

(defun overleaf-project--async-drain-completions ()
  "Run foreground callbacks for completed async operations."
  (dolist (completion (overleaf-project--async-pop-completions))
    (let ((key (overleaf-project--async-completion-key completion)))
      (when key
        (remhash key overleaf-project--async-locks)))
    (let ((default-directory
            (or (overleaf-project--async-completion-default-directory completion)
                default-directory))
          (overleaf-url
           (or (overleaf-project--async-completion-overleaf-url completion)
               overleaf-url)))
      (condition-case err
          (pcase (overleaf-project--async-completion-status completion)
            ('success
             (if-let ((callback
                       (overleaf-project--async-completion-on-success completion)))
                 (funcall callback
                          (overleaf-project--async-completion-value completion))
               (overleaf--message "Finished %s"
                                  (overleaf-project--async-completion-name
                                   completion))))
            ('error
             (let ((message
                    (overleaf-project--async-completion-error completion)))
               (if-let ((callback
                         (overleaf-project--async-completion-on-error completion)))
                   (funcall callback message)
                 (overleaf--warn "%s failed: %s"
                                 (overleaf-project--async-completion-name completion)
                                 message)))))
        (error
         (overleaf--warn "%s callback failed: %s"
                         (overleaf-project--async-completion-name completion)
                         (error-message-string err))))))
  (overleaf-project--async-stop-timer-if-idle))

(cl-defun overleaf-project--async-start
    (name function &key key on-success on-error quiet)
  "Run FUNCTION as background operation NAME.

When KEY is non-nil, only one operation with that key may run at a
time.  ON-SUCCESS receives FUNCTION's return value in the foreground.
ON-ERROR receives an error message string in the foreground."
  (if (not (overleaf-project--async-command-enabled-p))
      (condition-case err
          (let ((value (funcall function)))
            (when on-success
              (funcall on-success value))
            value)
        (error
         (let ((message (error-message-string err)))
           (if on-error
               (funcall on-error message)
             (signal (car err) (cdr err))))))
    (when (and key (gethash key overleaf-project--async-locks))
      (user-error "Overleaf operation already running: %s"
                  (gethash key overleaf-project--async-locks)))
    (when key
      (puthash key name overleaf-project--async-locks))
    (let ((captured-default-directory default-directory)
          (captured-overleaf-url overleaf-url)
          (captured-current-cookies overleaf--current-cookies)
          (captured-process-environment process-environment))
      (overleaf-project--async-ensure-timer)
      (unless quiet
        (overleaf--message "Started %s in background" name))
      (make-thread
       (lambda ()
         (let ((default-directory captured-default-directory)
               (overleaf-url captured-overleaf-url)
               (overleaf--current-cookies captured-current-cookies)
               (process-environment captured-process-environment))
           (condition-case err
               (overleaf-project--async-push-completion
                (make-overleaf-project--async-completion
                 :name name
                 :key key
                 :status 'success
                 :value (funcall function)
                 :on-success on-success
                 :on-error on-error
                 :default-directory captured-default-directory
                 :overleaf-url captured-overleaf-url))
             (error
              (overleaf-project--async-push-completion
               (make-overleaf-project--async-completion
                :name name
                :key key
                :status 'error
                :error (error-message-string err)
                :on-success on-success
                :on-error on-error
                :default-directory captured-default-directory
                :overleaf-url captured-overleaf-url))))))
       name))))

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

(defun overleaf-project--background-thread-p ()
  "Return non-nil when running outside Emacs' main thread."
  (and (fboundp 'current-thread)
       (boundp 'main-thread)
       (not (eq (current-thread) main-thread))))

(defun overleaf-project--run-async-wait
    (program args directory env noerror)
  "Run PROGRAM with ARGS asynchronously and wait from a worker thread.
DIRECTORY, ENV, and NOERROR have the same meaning as in
`overleaf-project--run'."
  (let* ((default-directory (or directory default-directory))
         (process-environment (append env process-environment))
         (mutex (make-mutex "overleaf-project-command"))
         (process nil)
         (done nil)
         (status nil)
         (output-chunks nil)
         (result nil))
    (unwind-protect
        (setq
         result
         (progn
           (setq process
                 (make-process
                  :name "overleaf-project-command"
                  :buffer nil
                  :command (cons program args)
                  :connection-type 'pipe
                  :noquery t
                  :filter
                  (lambda (_proc string)
                    (mutex-lock mutex)
                    (unwind-protect
                        (push string output-chunks)
                      (mutex-unlock mutex)))
                  :sentinel
                  (lambda (proc _event)
                    (when (memq (process-status proc) '(exit signal))
                      (mutex-lock mutex)
                      (unwind-protect
                          (unless done
                            (setq status (process-exit-status proc))
                            (setq done t))
                        (mutex-unlock mutex))))))
           (while (not done)
             (accept-process-output process 0.05)
             (thread-yield))
           (let ((output nil))
             (mutex-lock mutex)
             (unwind-protect
                 (setq output
                       (string-trim-right
                        (apply #'concat (nreverse output-chunks))))
               (mutex-unlock mutex))
             (unless (or noerror (and (integerp status) (zerop status)))
               (error "%s %s failed: %s"
                      program
                      (string-join args " ")
                      (if (string-empty-p output) "unknown error" output)))
             (make-overleaf-project--command-result
              :status status
              :output output))))
      (when (and process (process-live-p process))
        (ignore-errors (delete-process process))))
    result))

(defun overleaf-project--run (program args &optional directory env noerror)
  "Run PROGRAM with ARGS in DIRECTORY and optional ENV.
Return an `overleaf-project--command-result'.  Signal an error unless
NOERROR is non-nil."
  (let ((program (overleaf-project--ensure-executable program)))
    (if (and (overleaf-project--background-thread-p)
             (fboundp 'make-mutex))
        (overleaf-project--run-async-wait
         program
         args
         directory
         env
         noerror)
      (let ((default-directory (or directory default-directory))
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
             :output output)))))))

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
       (let ((result
              (overleaf-project--run
               "cmp"
               (list "--silent" left right)
               nil
               nil
               t)))
         (and (integerp (overleaf-project--command-result-status result))
              (zerop (overleaf-project--command-result-status result))))))

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

(defun overleaf-project--set-pending-pull-state (repo remote-commit)
  "Persist a pending pull state inside REPO recording REMOTE-COMMIT."
  (overleaf-project--git-config-set
   repo "overleaf.pendingRemoteCommit" remote-commit)
  (overleaf-project--git-config-set
   repo "overleaf.pendingAction" "pull"))

(defun overleaf-project--pending-state (repo)
  "Return pending push/pull metadata for REPO, or nil."
  (when-let* ((action-str
               (overleaf-project--git-config-get repo "overleaf.pendingAction")))
    (let ((action (intern action-str)))
      `(:action ,action
        :sync-branch
        ,(overleaf-project--git-config-get repo "overleaf.pendingSyncBranch")
        :original-branch
        ,(overleaf-project--git-config-get repo "overleaf.pendingOriginalBranch")
        :original-head
        ,(overleaf-project--git-config-get repo "overleaf.pendingOriginalHead")
        :remote-commit
        ,(overleaf-project--git-config-get repo "overleaf.pendingRemoteCommit")))))

;;;; Local safety backups

(defun overleaf-project--local-backup-ref-prefix ()
  "Return the configured local backup ref prefix."
  (let ((prefix
         (string-trim-right
          (string-trim overleaf-project-local-backup-ref-prefix)
          "/")))
    (cond
     ((string-empty-p prefix)
      (user-error "`overleaf-project-local-backup-ref-prefix' cannot be empty"))
     ((not (string-prefix-p "refs/" prefix))
      (user-error "`overleaf-project-local-backup-ref-prefix' must start with `refs/'"))
     (t prefix))))

(defun overleaf-project--sanitize-ref-component (value)
  "Return VALUE sanitized for use as one Git ref path component."
  (let ((component
         (replace-regexp-in-string
          "\\.\\.+"
          "-"
          (replace-regexp-in-string
           "[^[:alnum:]._-]+"
           "-"
           (downcase (string-trim (format "%s" value)))))))
    (setq component (string-trim component "[-.]+" "[-.]+"))
    (if (string-empty-p component)
        "unknown"
      component)))

(defun overleaf-project--local-backup-ref-name (repo target reason)
  "Return an unused backup ref name in REPO for TARGET and REASON."
  (let* ((prefix (overleaf-project--local-backup-ref-prefix))
         (branch
          (or (overleaf-project--git-output-noerror repo "branch" "--show-current")
              "detached"))
         (base
          (format
           "%s/%s-%s-%s-%s"
           prefix
           (format-time-string "%Y%m%d-%H%M%S")
           (overleaf-project--sanitize-ref-component branch)
           (overleaf-project--sanitize-ref-component reason)
           (substring target 0 (min 12 (length target))))))
    (cl-loop
     for index from 0
     for ref = (if (zerop index) base (format "%s-%d" base index))
     unless (overleaf-project--rev-parse-noerror repo ref)
     return ref)))

(defun overleaf-project--create-local-backup-ref
    (repo reason &optional revision)
  "Create a local backup ref in REPO for REVISION before REASON.
REVISION defaults to HEAD.  Return the created ref, or nil if backups are
disabled or the revision does not exist."
  (when overleaf-project-local-backups-enabled
    (when-let* ((target
                 (overleaf-project--rev-parse-noerror
                  repo
                  (or revision "HEAD")))
                (ref (overleaf-project--local-backup-ref-name
                      repo
                      target
                      reason)))
      (overleaf-project--git-output
       repo
       "update-ref"
       "-m"
       (format "overleaf: backup before %s" reason)
       ref
       target)
      (overleaf--debug "Created local backup ref %s at %s" ref target)
      ref)))


(provide 'overleaf-project-core)

;;; overleaf-project-core.el ends here
