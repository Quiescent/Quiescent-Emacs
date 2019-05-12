;;; buffer-disjunction --- produce the lines which aren't in both of two given buffers -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar *buffer-disjunction-results-buffer-name* "*disjunction-results*"
  "The name of the buffer to display disjunction results in.")

(require 'cl-lib)

(defun buffer-disjunction-lines-from-buffer (buffer)
  "Produce the lines in BUFFER as a list of strings."
  (with-current-buffer buffer
    (split-string (buffer-substring (point-min) (point-max))
                  "\n"
                  " ")))

(defun buffer-disjunction-buffer-disjunction (this-buffer that-buffer)
  "Produce lines which are not in both THIS-BUFFER and THAT-BUFFER.

  Results are displayed in a new buffer controlled by
  `*buffer-disjunction-results-buffer-name*'."
  (interactive "bThis buffer: \nbThat buffer:")
  (let* ((these-lines    (buffer-disjunction-lines-from-buffer this-buffer))
         (those-lines    (buffer-disjunction-lines-from-buffer that-buffer))
         (results-buffer (get-buffer *buffer-disjunction-results-buffer-name*))
         (diff           (mapcar (lambda (x) (concat x "\n"))
                                 (cl-set-difference these-lines those-lines))))
    (switch-to-buffer (if (not (bufferp results-buffer))
                          (generate-new-buffer *buffer-disjunction-results-buffer-name*)
                          results-buffer))
    (erase-buffer)
    (mapc #'insert diff)))

(provide 'quiescent/buffer-disjunction)
;;; buffer-disjunction ends here
