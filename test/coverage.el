;;; coverage.el --- Batch coverage runner for overleaf-project -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'testcover)

(defvar overleaf-project-coverage-min 0
  "Minimum total coverage percentage required by the batch coverage run.")

(defvar overleaf-project-coverage-directory "coverage"
  "Directory where batch coverage reports are written.")

(defconst overleaf-project-coverage--source-files
  '("overleaf-project-log.el"
    "overleaf-project-core.el"
    "overleaf-project-http.el"
    "overleaf-project-sync.el"
    "overleaf-project-firefox.el"
    "overleaf-project-auth.el"
    "overleaf-project.el"
    "overleaf-project-magit.el")
  "Source files instrumented by the batch coverage run.")

(defconst overleaf-project-coverage--test-files
  '("test/overleaf-project-test.el"
    "test/overleaf-project-git-test.el"
    "test/overleaf-project-sync-tree-test.el"
    "test/overleaf-project-command-test.el"
    "test/overleaf-project-http-auth-test.el"
    "test/overleaf-project-async-test.el"
    "test/overleaf-project-magit-test.el")
  "ERT files loaded by the batch coverage run.")

(defvar overleaf-project-coverage--instrumented nil
  "Alist mapping source file names to instrumented definition symbols.")

(defun overleaf-project-coverage--instrument-file (file)
  "Instrument FILE with `testcover' and remember its definition symbols."
  (cl-letf (((symbol-function 'message)
             (lambda (&rest _args) nil)))
    (testcover-start file))
  (push (cons file (mapcar #'car edebug-form-data))
        overleaf-project-coverage--instrumented))

(defun overleaf-project-coverage--entry-covered-p (entry)
  "Return non-nil if testcover ENTRY is considered covered."
  (or (eq entry 'edebug-ok-coverage)
      (memq (car-safe entry) '(testcover-1value maybe noreturn))))

(defun overleaf-project-coverage--symbol-summary (symbol)
  "Return (TOTAL COVERED UNCOVERED) for SYMBOL's coverage vector."
  (let ((coverage (get symbol 'edebug-coverage))
        (total 0)
        (covered 0)
        (uncovered 0))
    (when (vectorp coverage)
      (dotimes (index (length coverage))
        (setq total (1+ total))
        (if (overleaf-project-coverage--entry-covered-p
             (aref coverage index))
            (setq covered (1+ covered))
          (setq uncovered (1+ uncovered)))))
    (list total covered uncovered)))

(defun overleaf-project-coverage--file-summary (file symbols)
  "Return a plist coverage summary for FILE and SYMBOLS."
  (let ((defs 0)
        (total 0)
        (covered 0)
        (uncovered 0))
    (dolist (symbol symbols)
      (pcase-let ((`(,sym-total ,sym-covered ,sym-uncovered)
                   (overleaf-project-coverage--symbol-summary symbol)))
        (when (> sym-total 0)
          (setq defs (1+ defs))
          (setq total (+ total sym-total))
          (setq covered (+ covered sym-covered))
          (setq uncovered (+ uncovered sym-uncovered)))))
    (list :file file
          :defs defs
          :total total
          :covered covered
          :uncovered uncovered
          :percent (if (zerop total)
                       100.0
                     (* 100.0 (/ (float covered) total))))))

(defun overleaf-project-coverage--summaries ()
  "Return coverage summaries for all instrumented files."
  (mapcar (lambda (entry)
            (overleaf-project-coverage--file-summary
             (car entry)
             (cdr entry)))
          (nreverse overleaf-project-coverage--instrumented)))

(defun overleaf-project-coverage--total-summary (summaries)
  "Return total coverage summary for SUMMARIES."
  (let ((defs 0)
        (total 0)
        (covered 0)
        (uncovered 0))
    (dolist (summary summaries)
      (setq defs (+ defs (plist-get summary :defs)))
      (setq total (+ total (plist-get summary :total)))
      (setq covered (+ covered (plist-get summary :covered)))
      (setq uncovered (+ uncovered (plist-get summary :uncovered))))
    (list :file "TOTAL"
          :defs defs
          :total total
          :covered covered
          :uncovered uncovered
          :percent (if (zerop total)
                       100.0
                     (* 100.0 (/ (float covered) total))))))

(defun overleaf-project-coverage--format-summary (summary)
  "Return a human-readable line for coverage SUMMARY."
  (format "%-28s defs=%3d forms=%5d covered=%5d missed=%5d %6.2f%%"
          (plist-get summary :file)
          (plist-get summary :defs)
          (plist-get summary :total)
          (plist-get summary :covered)
          (plist-get summary :uncovered)
          (plist-get summary :percent)))

(defun overleaf-project-coverage--write-tsv (summaries total)
  "Write SUMMARIES and TOTAL to the batch coverage TSV report."
  (make-directory overleaf-project-coverage-directory t)
  (let ((report (expand-file-name
                 "testcover-summary.tsv"
                 overleaf-project-coverage-directory)))
    (with-temp-file report
      (insert "file\tdefs\tforms\tcovered\tmissed\tpercent\n")
      (dolist (summary (append summaries (list total)))
        (insert
         (format "%s\t%d\t%d\t%d\t%d\t%.2f\n"
                 (plist-get summary :file)
                 (plist-get summary :defs)
                 (plist-get summary :total)
                 (plist-get summary :covered)
                 (plist-get summary :uncovered)
                 (plist-get summary :percent)))))
    report))

(defun overleaf-project-coverage-run ()
  "Run ERT tests under `testcover' and write a coverage summary."
  (setq overleaf-project-coverage--instrumented nil)
  (dolist (file overleaf-project-coverage--source-files)
    (overleaf-project-coverage--instrument-file file))
  (dolist (file overleaf-project-coverage--test-files)
    (load (expand-file-name file default-directory) nil t))
  (let* ((stats (ert-run-tests-batch t))
         (summaries (overleaf-project-coverage--summaries))
         (total (overleaf-project-coverage--total-summary summaries))
         (report (overleaf-project-coverage--write-tsv summaries total)))
    (princ "\nCoverage summary:\n")
    (dolist (summary summaries)
      (princ (concat (overleaf-project-coverage--format-summary summary)
                     "\n")))
    (princ (concat (overleaf-project-coverage--format-summary total)
                   "\n"))
    (princ (format "Coverage report: %s\n" report))
    (when (> (ert-stats-completed-unexpected stats) 0)
      (kill-emacs 1))
    (when (< (plist-get total :percent) overleaf-project-coverage-min)
      (princ
       (format
        "Coverage %.2f%% is below required minimum %.2f%%\n"
        (plist-get total :percent)
        (float overleaf-project-coverage-min)))
      (kill-emacs 1))))

(overleaf-project-coverage-run)

;;; coverage.el ends here
