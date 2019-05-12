;;; other-window --- Convenience functions for manipulating the other window -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defmacro quiescent-in-other-buffer (&rest body)
  "Execute BODY in the other window and return to the startindg window."
  `(progn
     (other-window 1)
     ,@body
     (select-window (previous-window))))

(defun quiescent-other-window-line-down ()
  "Move point one line down in the other window.

Maintains the point in the current window."
  (interactive)
  (quiescent-in-other-buffer
   (forward-line)))

(defun quiescent-other-window-line-up ()
  "Move point one line up in the other window.

Maintains the point in the current window."
  (interactive)
  (quiescent-in-other-buffer
   (forward-line -1)))

(defun quiescent-search-other-window ()
  "Search the other window and then pop back to the current one."
  (interactive)
  (progn
    (quiescent-in-other-buffer
     (isearch-forward))))

(provide 'quiescent/other-window)
;;; other-window ends here
