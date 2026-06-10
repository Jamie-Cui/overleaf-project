;;; overleaf-project-magit-test.el --- Magit helper tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'overleaf-project-magit)

(ert-deftest overleaf-project-magit-test-state-labels ()
  (cl-letf (((symbol-function 'overleaf-project--pending-state)
             (lambda (_repo) '(:action pull))))
    (should (equal (overleaf-project-magit--state-label
                    "/repo" t t t t)
                   '("pending pull" . error))))
  (cl-letf (((symbol-function 'overleaf-project--pending-state)
             (lambda (_repo) nil)))
    (should (equal (overleaf-project-magit--state-label
                    "/repo" t nil nil nil)
                   '("in sync" . magit-dimmed)))
    (should (equal (overleaf-project-magit--state-label
                    "/repo" t t nil nil)
                   '("remote changes" . warning)))
    (should (equal (overleaf-project-magit--state-label
                    "/repo" nil t nil t)
                   '("local matches remote" . warning)))
    (should (equal (overleaf-project-magit--state-label
                    "/repo" nil t t nil)
                   '("local changes" . warning)))
    (should (equal (overleaf-project-magit--state-label
                    "/repo" nil t nil nil)
                   '("local and remote changes" . warning)))
    (should (equal (overleaf-project-magit--state-label
                    "/repo" nil nil nil nil)
                   '("local changes" . warning)))))

(ert-deftest overleaf-project-magit-test-fresh-remote-commit-cache ()
  (with-temp-buffer
    (setq overleaf-project-magit--remote-commit "remote")
    (setq overleaf-project-magit--remote-base-rev "base")
    (setq overleaf-project-magit--last-remote-refresh-time 123)
    (should (equal (overleaf-project-magit--fresh-remote-commit "base")
                   "remote"))
    (should (equal overleaf-project-magit--last-remote-refresh-time 123))
    (should-not (overleaf-project-magit--fresh-remote-commit "other-base"))
    (should-not overleaf-project-magit--remote-commit)
    (should-not overleaf-project-magit--remote-base-rev)
    (should-not overleaf-project-magit--last-remote-refresh-time)))

(ert-deftest overleaf-project-magit-test-auto-refresh-due-p ()
  (with-temp-buffer
    (setq overleaf-project-magit--last-remote-refresh-time nil)
    (should (overleaf-project-magit--auto-refresh-due-p))
    (setq overleaf-project-magit--last-remote-refresh-time (float-time))
    (should-not (overleaf-project-magit--auto-refresh-due-p))
    (setq overleaf-project-magit--last-remote-refresh-time
          (- (float-time)
             overleaf-project-magit--auto-refresh-remote-interval
             1))
    (should (overleaf-project-magit--auto-refresh-due-p))))

(ert-deftest overleaf-project-magit-test-maybe-auto-refresh-remote ()
  (with-temp-buffer
    (let ((overleaf-project-magit--suppress-next-auto-refresh t)
          (called nil))
      (cl-letf (((symbol-function 'overleaf-project-magit-refresh-remote)
                 (lambda () (setq called t))))
        (overleaf-project-magit--maybe-auto-refresh-remote)
        (should-not called)
        (should-not overleaf-project-magit--suppress-next-auto-refresh)))
    (let ((overleaf-project-magit-auto-refresh-remote t)
          (overleaf-project-magit--refreshing nil)
          (overleaf-project-magit--last-remote-refresh-time nil)
          (called nil))
      (cl-letf (((symbol-function 'overleaf-project--async-enabled-p)
                 (lambda () t))
                ((symbol-function 'derived-mode-p)
                 (lambda (&rest _modes) t))
                ((symbol-function 'magit-toplevel)
                 (lambda () "/repo"))
                ((symbol-function 'overleaf-project--managed-repo-p)
                 (lambda (_repo) t))
                ((symbol-function 'overleaf-project-magit-refresh-remote)
                 (lambda () (setq called t))))
        (overleaf-project-magit--maybe-auto-refresh-remote)
        (should called)))
    (let ((overleaf-project-magit-auto-refresh-remote nil)
          (called nil))
      (cl-letf (((symbol-function 'overleaf-project-magit-refresh-remote)
                 (lambda () (setq called t))))
        (overleaf-project-magit--maybe-auto-refresh-remote)
        (should-not called)))))

(ert-deftest overleaf-project-magit-test-finish-remote-refresh ()
  (let ((buffer (generate-new-buffer " *overleaf-magit-test*"))
        (refreshed nil)
        (messages nil))
    (unwind-protect
        (cl-letf (((symbol-function 'magit-refresh)
                   (lambda () (setq refreshed t)))
                  ((symbol-function 'overleaf-project--message)
                   (lambda (&rest args) (push args messages))))
          (with-current-buffer buffer
            (setq overleaf-project-magit--refreshing t)
            (setq overleaf-project-magit--remote-commit nil)
            (setq overleaf-project-magit--remote-base-rev nil)
            (setq overleaf-project-magit--suppress-next-auto-refresh nil))
          (overleaf-project-magit--finish-remote-refresh
           buffer
           "remote-commit"
           "base-rev"
           "Ready")
          (with-current-buffer buffer
            (should (equal overleaf-project-magit--remote-commit
                           "remote-commit"))
            (should (equal overleaf-project-magit--remote-base-rev
                           "base-rev"))
            (should-not overleaf-project-magit--refreshing)
            (should overleaf-project-magit--suppress-next-auto-refresh))
          (should refreshed)
          (should (equal messages '(("%s" "Ready")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest overleaf-project-magit-test-enable-status-buffer-hooks ()
  (with-temp-buffer
    (let ((magit-refresh-buffer-hook nil))
      (overleaf-project-magit--enable-status-buffer-hooks)
      (should (memq #'overleaf-project-magit--maybe-auto-refresh-remote
                    magit-refresh-buffer-hook)))))

;;; overleaf-project-magit-test.el ends here
