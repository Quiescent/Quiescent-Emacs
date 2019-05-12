;;; editing --- Useful commands for editing text -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Unfill REGION, otherwise paragraph at point.

i.e. the reverse of fill paragraph."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

(provide 'quiescent/editing)
;;; editing ends here
