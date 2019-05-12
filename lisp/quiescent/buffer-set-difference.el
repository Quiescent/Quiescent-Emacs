;;; buffer-set-difference --- produce the lines which are in the first given buffer but not the second -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar *buffer-set-difference-results-buffer-name* "*this-but-not-that-results*"
  "The name of the buffer to display this-but-not-that results in.")

(require 'quiescent/buffer-disjunction)

(defun buffer-set-difference (this-buffer that-buffer)
  "Produce lines which are in THIS-BUFFER but not THAT-BUFFER.

  Results are displayed in a new buffer controlled by
    `*buffer-set-difference-results-buffer-name*'."
  (interactive "bThis buffer: \nbThat buffer:")
  (let* ((these-lines    (buffer-disjunction-lines-from-buffer this-buffer))
         (those-lines    (mapcar (lambda (x) (cons x t))
                                 (buffer-disjunction-lines-from-buffer that-buffer)))
         (results-buffer (get-buffer *buffer-set-difference-results-buffer-name*))
         (diff           (mapcar (lambda (x) (concat x "\n"))
                                 (cl-remove-if (lambda (x) (assoc x those-lines))
                                               these-lines))))
    (switch-to-buffer (if (not (bufferp results-buffer))
                          (generate-new-buffer *buffer-set-difference-results-buffer-name*)
                          results-buffer))
    (erase-buffer)
    (mapc #'insert diff)))

(provide 'quiescent/buffer-set-difference)
;;; buffer-set-difference ends here
