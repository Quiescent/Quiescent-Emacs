;;; quiescent/hippie-expand-function --- Expand the function at point with hippie expand -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun quiescent-previous-char ()
  "Produce the character one before point."
  (save-excursion
    (backward-char)
    (thing-at-point 'char t)))

(defun quiescent-start-of-symbol ()
  "Produce the point at start of the symbol around point."
  (save-excursion
    (when (not (string-equal "." (quiescent-previous-char)))
      (backward-sexp))
    (point)))

(defun quiescent-end-of-symbol ()
  "Produce the points at the end of the symbol around point."
  (save-excursion
    ;; (when (not (= ?\ (thing-at-point 'char)))
    ;;   (forward-sexp))
    (point)))

(provide 'quiescent/hippie-expand-function)
;;; hippie-expand-function ends here
