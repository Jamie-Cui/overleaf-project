;;; overleaf-project-async-test.el --- Async state tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'overleaf-project-core)

(defmacro overleaf-project-async-test--with-clean-state (&rest body)
  "Run BODY with isolated async globals."
  (declare (indent 0) (debug t))
  `(let ((overleaf-project-enable-async nil)
         (overleaf-project--async-locks (make-hash-table :test #'equal))
         (overleaf-project--async-tasks (make-hash-table :test #'eql))
         (overleaf-project--async-canceled-task-ids
          (make-hash-table :test #'eql))
         (overleaf-project--async-next-task-id 0)
         (overleaf-project--async-completions nil)
         (overleaf-project--async-current-task-id nil)
         (overleaf-project--async-timer nil))
     ,@body))

(ert-deftest overleaf-project-async-test-enabled-p-respects-noninteractive ()
  (let ((overleaf-project-enable-async t)
        (noninteractive t))
    (should-not (overleaf-project--async-enabled-p)))
  (let ((overleaf-project-enable-async nil)
        (noninteractive nil))
    (should-not (overleaf-project--async-enabled-p))))

(ert-deftest overleaf-project-async-test-start-synchronous-success-and-error ()
  (overleaf-project-async-test--with-clean-state
    (let ((success nil)
          (error-message nil))
      (should (equal (overleaf-project--async-start
                      "success"
                      (lambda () 42)
                      :on-success (lambda (value) (setq success value)))
                     42))
      (should (equal success 42))
      (should-not error-message)
      (should (equal (overleaf-project--async-start
                      "error"
                      (lambda () (error "boom"))
                      :on-error (lambda (message)
                                  (setq error-message message)))
                     "boom"))
      (should (equal error-message "boom"))
      (should-error
       (overleaf-project--async-start
        "error"
        (lambda () (error "uncaught")))
       :type 'error))))

(ert-deftest overleaf-project-async-test-register-and-remove-tasks ()
  (overleaf-project-async-test--with-clean-state
    (let ((task (make-overleaf-project--async-task
                 :id 1
                 :name "task"
                 :key "key")))
      (should (overleaf-project--async-lock-empty-p))
      (should (overleaf-project--async-task-empty-p))
      (puthash "key" "task" overleaf-project--async-locks)
      (overleaf-project--async-register-task task)
      (should-not (overleaf-project--async-lock-empty-p))
      (should-not (overleaf-project--async-task-empty-p))
      (let ((overleaf-project--async-current-task-id 1))
        (should (eq (overleaf-project--async-current-task) task))
        (should-not (overleaf-project--async-current-task-canceled-p))
        (setf (overleaf-project--async-task-canceled task) t)
        (should (overleaf-project--async-current-task-canceled-p)))
      (overleaf-project--async-remove-task 1)
      (remhash "key" overleaf-project--async-locks)
      (should (overleaf-project--async-lock-empty-p))
      (should (overleaf-project--async-task-empty-p)))))

(ert-deftest overleaf-project-async-test-completion-queue-and-drain ()
  (overleaf-project-async-test--with-clean-state
    (let ((events nil)
          (warnings nil))
      (puthash "key-a" "operation-a" overleaf-project--async-locks)
      (puthash "key-b" "operation-b" overleaf-project--async-locks)
      (cl-letf (((symbol-function 'overleaf-project--warn)
                 (lambda (&rest args)
                   (push args warnings)))
                ((symbol-function 'overleaf-project--async-stop-timer-if-idle)
                 (lambda () nil)))
        (overleaf-project--async-push-completion
         (make-overleaf-project--async-completion
          :name "operation-a"
          :key "key-a"
          :status 'success
          :value "value"
          :on-success (lambda (value)
                        (push (list :success value) events))))
        (overleaf-project--async-push-completion
         (make-overleaf-project--async-completion
          :name "operation-b"
          :key "key-b"
          :status 'error
          :error "failed"
          :on-error (lambda (message)
                      (push (list :error message) events))))
        (overleaf-project--async-drain-completions)
        (should (equal (nreverse events)
                       '((:success "value") (:error "failed"))))
        (should-not warnings)
        (should-not (gethash "key-a" overleaf-project--async-locks))
        (should-not (gethash "key-b" overleaf-project--async-locks)))))

(ert-deftest overleaf-project-async-test-completion-default-handlers ()
  (overleaf-project-async-test--with-clean-state
    (let ((messages nil)
          (warnings nil))
      (cl-letf (((symbol-function 'overleaf-project--message)
                 (lambda (&rest args) (push args messages)))
                ((symbol-function 'overleaf-project--warn)
                 (lambda (&rest args) (push args warnings)))
                ((symbol-function 'overleaf-project--async-stop-timer-if-idle)
                 (lambda () nil)))
        (overleaf-project--async-push-completion
         (make-overleaf-project--async-completion
          :name "success-op"
          :status 'success
          :value "ignored"))
        (overleaf-project--async-push-completion
         (make-overleaf-project--async-completion
          :name "error-op"
          :status 'error
          :error "bad"))
        (overleaf-project--async-drain-completions)
        (should (member '("Finished %s" "success-op") messages))
        (should (member '("%s failed: %s" "error-op" "bad") warnings))))))

(ert-deftest overleaf-project-async-test-cancel-completion-removes-task-state ()
  (overleaf-project-async-test--with-clean-state
    (let ((task (make-overleaf-project--async-task
                 :id 7
                 :name "task")))
      (overleaf-project--async-register-task task)
      (puthash 7 t overleaf-project--async-canceled-task-ids)
      (should (overleaf-project--async-cancel-completion-p
               (make-overleaf-project--async-completion
                :task-id 7
                :name "task")))
      (should-not (gethash 7 overleaf-project--async-tasks))
      (should-not (gethash 7 overleaf-project--async-canceled-task-ids)))))

(ert-deftest overleaf-project-async-test-force-stop-clears-state ()
  (overleaf-project-async-test--with-clean-state
    (should-error (overleaf-project--force-stop) :type 'user-error)
    (let ((messages nil)
          (task (make-overleaf-project--async-task
                 :id 1
                 :name "task"
                 :key "key")))
      (puthash "key" "task" overleaf-project--async-locks)
      (overleaf-project--async-register-task task)
      (setq overleaf-project--async-completions
            (list (make-overleaf-project--async-completion
                   :name "task")))
      (cl-letf (((symbol-function 'overleaf-project--message)
                 (lambda (&rest args)
                   (push args messages))))
        (overleaf-project--force-stop))
      (should (overleaf-project--async-lock-empty-p))
      (should (overleaf-project--async-task-empty-p))
      (should-not overleaf-project--async-completions)
      (should (equal (car messages)
                     '("Stopped %d Overleaf background operation%s" 1 "")))))))

;;; overleaf-project-async-test.el ends here
