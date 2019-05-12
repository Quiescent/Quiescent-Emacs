;;; quiescent/cross-link-mode --- link back to a document which symbols originated from  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar quiescent-cross-link-linked-buffer nil
  "The buffer which this buffer is cross linked to.")

(make-variable-buffer-local 'quiescent-cross-link-linked-buffer)

(defun quiescent-cross-link ()
  "Cross link symbols in the current buffer to another buffer."
  (interactive)
  (let ((search-symbol (thing-at-point 'symbol)))
    (when (not quiescent-cross-link-linked-buffer)
      (call-interactively 'quiescent-cross-link-set-cross-linked-buffer))
    (pop-to-buffer quiescent-cross-link-linked-buffer)
    (goto-char (point-min))
    (search-forward search-symbol)))

(defun quiescent-coss-link-reset ()
  "Reset the linked buffer for current buffer."
  (interactive)
  (setq quiescent-cross-link-linked-buffer nil))

(defun quiescent-cross-link-set-cross-linked-buffer (linked-buffer-name)
  "Set the LINKED-BUFFER-NAME of the buffer to link this buffer to."
  (interactive "bEnter buffer name to link to: ")
  (setq quiescent-cross-link-linked-buffer linked-buffer-name))

(provide 'quiescent/cross-link-mode)
;;; cross-link-mode ends here

