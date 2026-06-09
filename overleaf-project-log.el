;;; overleaf-project-log.el --- Logging for overleaf-project -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 Jamie Cui
;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Assisted-by: Codex:GPT-5.5
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Global log buffer and logging helpers for overleaf-project.

;;; Code:

;;;; Customization

(defgroup overleaf-project nil
  "Clone, push, and pull full Overleaf projects."
  :prefix "overleaf-project-"
  :group 'tools)

(defcustom overleaf-project-debug nil
  "Whether to emit verbose debug messages."
  :type 'boolean
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

(defconst overleaf-project-log--buffer-name "*overleaf-project-log*"
  "Name of the global Overleaf project log buffer.")

(defconst overleaf-project-log--time-format "%Y-%m-%d %H:%M:%S"
  "Time format used for entries in the Overleaf project log buffer.")

(defvar overleaf-project-log--mutex
  (and (fboundp 'make-mutex)
       (make-mutex "overleaf-project-log"))
  "Mutex protecting writes to the Overleaf project log buffer.")

(defun overleaf-project-log-make-context (&rest keys)
  "Return a normalized log context plist.
KEYS accepts `:project-name', `:project-id', `:repo', and `:url'."
  (let ((project-name (plist-get keys :project-name))
        (project-id (plist-get keys :project-id))
        (repo (plist-get keys :repo))
        (url (plist-get keys :url)))
    (append
     (when project-name
       (list :project-name project-name))
     (when project-id
       (list :project-id project-id))
     (when repo
       (list :repo (directory-file-name (expand-file-name repo))))
     (when url
       (list :url url)))))

(defun overleaf-project-log--merge-contexts (&rest contexts)
  "Merge context plists in CONTEXTS.
Later non-nil values replace earlier values."
  (let (merged)
    (dolist (context contexts)
      (when (listp context)
        (while context
          (let ((key (pop context))
                (value (pop context)))
            (when (and (keywordp key) value)
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

(defun overleaf-project-log--context-label (context)
  "Return a display label for CONTEXT."
  (let* ((project-name (plist-get context :project-name))
         (project-id (plist-get context :project-id))
         (repo (plist-get context :repo))
         (url (plist-get context :url))
         (parts
          (delq
           nil
           (list
            (when project-name
              (format "project=%s" project-name))
            (when project-id
              (format "id=%s" project-id))
            (when repo
              (format "repo=%s"
                      (abbreviate-file-name
                       (directory-file-name (expand-file-name repo)))))
            (when url
              (format "url=%s" url))))))
    (if parts
        (mapconcat #'identity parts " ")
      "global")))

;;;; Buffer

(define-derived-mode overleaf-project-log-mode special-mode "Overleaf-Project-Log"
  "Major mode for the global Overleaf project log buffer."
  (setq-local truncate-lines t))

(defun overleaf-project-log--buffer ()
  "Return the global Overleaf project log buffer."
  (let ((buffer (get-buffer-create overleaf-project-log--buffer-name)))
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

(defmacro overleaf-project-log--with-mutex (&rest body)
  "Run BODY while holding the log mutex when available."
  (declare (indent 0) (debug t))
  `(if overleaf-project-log--mutex
       (progn
         (mutex-lock overleaf-project-log--mutex)
         (unwind-protect
             (progn ,@body)
           (mutex-unlock overleaf-project-log--mutex)))
     ,@body))

(defun overleaf-project-log--append (level text)
  "Append TEXT as LEVEL to the global Overleaf project log buffer."
  (let* ((context (overleaf-project-log-current-context))
         (timestamp (format-time-string overleaf-project-log--time-format))
         (prefix (format "%s %-5s [%s] "
                         timestamp
                         (upcase (symbol-name level))
                         (overleaf-project-log--context-label context)))
         (lines (split-string (or text "") "\n"))
         (buffer (overleaf-project-log--buffer)))
    (overleaf-project-log--with-mutex
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (moving (= (point) (point-max))))
          (save-excursion
            (goto-char (point-max))
            (insert prefix (car lines) "\n")
            (dolist (line (cdr lines))
              (insert (make-string (length prefix) ?\s) line "\n")))
          (when moving
            (goto-char (point-max))))))))

(defun overleaf-project-log--emit (level echo-prefix format-string args)
  "Log LEVEL entry and echo it with ECHO-PREFIX when enabled."
  (let ((text (apply #'format format-string args)))
    (overleaf-project-log--append level text)
    (when overleaf-project-log-echo
      (message "%s" (concat "[overleaf-project] " echo-prefix text)))
    text))

;;;; Logging helpers

(defun overleaf-project--message (format-string &rest args)
  "Log an Overleaf info message using FORMAT-STRING and ARGS."
  (overleaf-project-log--emit 'info "" format-string args))

(defun overleaf-project--warn (format-string &rest args)
  "Log an Overleaf warning using FORMAT-STRING and ARGS."
  (overleaf-project-log--emit 'warn "WARNING: " format-string args))

(defun overleaf-project--debug (format-string &rest args)
  "Log a debug message using FORMAT-STRING and ARGS."
  (when overleaf-project-debug
    (overleaf-project-log--emit 'debug "DEBUG: " format-string args)))

(provide 'overleaf-project-log)

;;; overleaf-project-log.el ends here
