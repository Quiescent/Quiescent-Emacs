;;; quiescent/sql --- Setup and convenience functions for editing sql -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar quiescent-sql-completion-candidates '("SELECT" "FROM" "WHERE" "ORDER" "GROUP" "BY" "UPDATE" "DELETE")
  "A variable list of candidates to complete a sql symbol from.")

(defvar quiescent-sql-original-completion-candidates '("SELECT" "FROM" "WHERE" "ORDER" "GROUP" "BY" "UPDATE" "DELETE")
  "A fixed list of candidates to complete a sql symbol from.")

(require 'thingatpt)
(require 'sql)

(use-package flx
  :ensure t)
(use-package flx-ido
  :ensure t)

(defun quiescent-fuzzy-completion (prefix completions)
  "Complete the given PREFIX using a fixed list of COMPLETIONS.

Part of this is ripped out of the implementation of flx's
ido matching and sorting.  See `flx-ido-match-internal'."
  (let ((flex-result (flx-flex-match prefix completions)))
    (if (< (length flex-result) flx-ido-threshold)
        (let* ((matches (cl-loop for item in flex-result
                                 for string = (ido-name item)
                                 for score = (flx-score string prefix flx-file-cache)
                                 if score
                                 collect (cons item score)
                                 into matches
                                 finally return matches)))
          (mapcar 'car (delete-consecutive-dups
                        (sort matches
                              (lambda (x y) (> (cadr x) (cadr y))))
                        t)))
      (mapcar 'car  flex-result))))

(defun quiescent-sql-completion ()
  "Complete PREFIX fuzzily using a fixed list of completions.

The fixed list is read from a variable:
`quiescent-sql-completion-candidates', which ought to be set when
connecting to a given database."
  (let* ((prefix        (thing-at-point           'symbol))
         (prefix-bounds (bounds-of-thing-at-point 'symbol))
         (candidates
          (if (looking-at "^")
              (quiescent-fuzzy-completion prefix quiescent-sql-original-completion-candidates)
            (quiescent-fuzzy-completion prefix quiescent-sql-completion-candidates))))
    (if (null candidates)
        (progn (message "No candidates for completion found")
               nil)
      (list (car prefix-bounds) (cdr prefix-bounds) candidates))))

(defun quiescent-register-sql-completion ()
  "Register the sql completion function as a completion method in this buffer."
  (when (null quiescent-starting-up)
    (progn
      (setq completion-at-point-functions (list #'quiescent-sql-completion))
      (setq completion-ignore-case        t))))

(add-hook 'sql-mode-hook #'quiescent-register-sql-completion)

(defun quiescent-describe-table-at-point ()
  "Desicribe the table at point."
  (interactive)
  (sql-send-string (format "DESCRIBE %s;"
                           (thing-at-point 'symbol t))))

(define-key sql-mode-map (kbd "C-c C-d")
  #'quiescent-describe-table-at-point)

(provide 'quiescent/sql)
;;; sql ends here
