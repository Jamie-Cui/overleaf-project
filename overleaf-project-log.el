;;; overleaf-project-log.el --- Logging for overleaf-project -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Global log buffer and logging helpers for overleaf-project.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)

;;;; Customization

(defgroup overleaf-project nil
  "Clone, push, and pull full Overleaf projects."
  :prefix "overleaf-project-"
  :group 'tools)

(defcustom overleaf-project-debug nil
  "Whether to emit verbose debug messages."
  :type 'boolean
  :group 'overleaf-project)

(defcustom overleaf-project-log-buffer-name "*overleaf-project-log*"
  "Name of the global Overleaf project log buffer."
  :type 'string
  :group 'overleaf-project)

(defcustom overleaf-project-log-time-format "%Y-%m-%d %H:%M:%S"
  "Time format used for entries in `overleaf-project-log-buffer-name'."
  :type 'string
  :group 'overleaf-project)

(defcustom overleaf-project-log-echo t
  "Whether Overleaf project log entries are also echoed in the minibuffer."
  :type 'boolean
  :group 'overleaf-project)

;;;; Context

(defvar overleaf-project-log-context nil
  "Dynamic plist describing the Overleaf project currently being logged.
Supported keys include `:project-name', `:project-id', `:repo', and
`:url'.")

(defvar overleaf-project-log-context-function nil
  "Function returning a fallback Overleaf project log context plist.")

(defvar overleaf-project-log--mutex
  (and (fboundp 'make-mutex)
       (make-mutex "overleaf-project-log"))
  "Mutex protecting writes to the Overleaf project log buffer.")

(cl-defun overleaf-project-log-make-context
    (&key project-name project-id repo url)
  "Return a normalized log context plist.
PROJECT-NAME, PROJECT-ID, REPO, and URL identify the current Overleaf
project or server."
  (let (context)
    (when project-name
      (setq context (plist-put context :project-name project-name)))
    (when project-id
      (setq context (plist-put context :project-id project-id)))
    (when repo
      (setq context
            (plist-put context
                       :repo
                       (directory-file-name (expand-file-name repo)))))
    (when url
      (setq context (plist-put context :url url)))
    context))

(defun overleaf-project-log--plist-p (value)
  "Return non-nil if VALUE looks like a plist."
  (and (listp value)
       (cl-evenp (length value))
       (cl-loop for (key _value) on value by #'cddr
                always (keywordp key))))

(defun overleaf-project-log--normalize-context (context)
  "Normalize CONTEXT into a log context plist."
  (cond
   ((null context) nil)
   ((stringp context)
    (overleaf-project-log-make-context :project-name context))
   ((overleaf-project-log--plist-p context)
    (cl-loop for (key value) on context by #'cddr
             when value append (list key value)))
   (t nil)))

(defun overleaf-project-log--merge-contexts (&rest contexts)
  "Merge CONTEXTS, with later non-nil keys taking precedence."
  (let (merged)
    (dolist (context contexts)
      (let ((normalized (overleaf-project-log--normalize-context context)))
        (while normalized
          (let ((key (pop normalized))
                (value (pop normalized)))
            (when value
              (setq merged (plist-put merged key value)))))))
    merged))

(defun overleaf-project-log-current-context ()
  "Return the current effective Overleaf project log context."
  (overleaf-project-log--merge-contexts
   (and overleaf-project-log-context-function
        (ignore-errors (funcall overleaf-project-log-context-function)))
   overleaf-project-log-context))

(defmacro overleaf-project-log-with-context (context &rest body)
  "Run BODY with CONTEXT merged into the current log context."
  (declare (indent 1) (debug (form body)))
  `(let ((overleaf-project-log-context
          (overleaf-project-log--merge-contexts
           (overleaf-project-log-current-context)
           ,context)))
     ,@body))

(defun overleaf-project-log--url-host (url)
  "Return the host part of URL, or URL when it cannot be parsed."
  (when (stringp url)
    (or (ignore-errors (url-host (url-generic-parse-url url)))
        url)))

(defun overleaf-project-log--context-label (context)
  "Return a display label for CONTEXT."
  (let* ((project-name (plist-get context :project-name))
         (project-id (plist-get context :project-id))
         (repo (plist-get context :repo))
         (url (overleaf-project-log--url-host (plist-get context :url)))
         (parts nil))
    (when project-name
      (push (format "project=%s" project-name) parts))
    (when project-id
      (push (format "id=%s" project-id) parts))
    (when repo
      (push (format "repo=%s"
                    (abbreviate-file-name
                     (directory-file-name (expand-file-name repo))))
            parts))
    (when url
      (push (format "url=%s" url) parts))
    (if parts
        (string-join (nreverse parts) " ")
      "global")))

;;;; Buffer

(define-derived-mode overleaf-project-log-mode special-mode "Overleaf-Project-Log"
  "Major mode for the global Overleaf project log buffer."
  (setq-local truncate-lines t))

(defun overleaf-project-log--buffer ()
  "Return the global Overleaf project log buffer."
  (let ((buffer (get-buffer-create overleaf-project-log-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'overleaf-project-log-mode)
        (overleaf-project-log-mode)))
    buffer))

;;;###autoload
(defun overleaf-project-log ()
  "Display the global Overleaf project log buffer."
  (interactive)
  (display-buffer (overleaf-project-log--buffer)))

;;;###autoload
(defun overleaf-project-log-clear ()
  "Clear the global Overleaf project log buffer."
  (interactive)
  (let ((buffer (overleaf-project-log--buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))))

(defun overleaf-project-log--with-mutex (function)
  "Call FUNCTION while holding the log mutex when available."
  (if overleaf-project-log--mutex
      (progn
        (mutex-lock overleaf-project-log--mutex)
        (unwind-protect
            (funcall function)
          (mutex-unlock overleaf-project-log--mutex)))
    (funcall function)))

(defun overleaf-project-log--level-name (level)
  "Return display name for LEVEL."
  (upcase (symbol-name level)))

(defun overleaf-project-log--append (level text)
  "Append TEXT as LEVEL to the global Overleaf project log buffer."
  (let* ((context (overleaf-project-log-current-context))
         (timestamp (format-time-string overleaf-project-log-time-format))
         (prefix (format "%s %-5s [%s] "
                         timestamp
                         (overleaf-project-log--level-name level)
                         (overleaf-project-log--context-label context)))
         (lines (split-string (or text "") "\n"))
         (buffer (overleaf-project-log--buffer)))
    (overleaf-project-log--with-mutex
     (lambda ()
       (with-current-buffer buffer
         (let ((inhibit-read-only t)
               (moving (= (point) (point-max))))
           (save-excursion
             (goto-char (point-max))
             (insert prefix (car lines) "\n")
             (dolist (line (cdr lines))
               (insert (make-string (length prefix) ?\s) line "\n")))
           (when moving
             (goto-char (point-max)))))))))

(defun overleaf-project-log--format (format-string args)
  "Format FORMAT-STRING with ARGS for logging."
  (apply #'format format-string args))

(defun overleaf-project-log--record (level format-string args)
  "Record a LEVEL log entry using FORMAT-STRING and ARGS.
Return the formatted message text."
  (let ((text (overleaf-project-log--format format-string args)))
    (overleaf-project-log--append level text)
    text))

;;;; Logging helpers

(defun overleaf-project--message (format-string &rest args)
  "Log an Overleaf info message using FORMAT-STRING and ARGS."
  (let ((text (overleaf-project-log--record 'info format-string args)))
    (when overleaf-project-log-echo
      (message "%s" (concat "[overleaf-project] " text)))
    text))

(defun overleaf-project--warn (format-string &rest args)
  "Log an Overleaf warning using FORMAT-STRING and ARGS."
  (let ((text (overleaf-project-log--record 'warn format-string args)))
    (when overleaf-project-log-echo
      (message "%s" (concat "[overleaf-project] WARNING: " text)))
    text))

(defun overleaf-project--debug (format-string &rest args)
  "Log a debug message using FORMAT-STRING and ARGS."
  (when overleaf-project-debug
    (let ((text (overleaf-project-log--record 'debug format-string args)))
      (when overleaf-project-log-echo
        (message "%s" (concat "[overleaf-project] DEBUG: " text)))
      text)))

(provide 'overleaf-project-log)

;;; overleaf-project-log.el ends here
