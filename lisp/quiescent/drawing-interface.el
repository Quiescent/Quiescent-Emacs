;;; quiescent/drawing-interface --- An interface for simple drawing in ASCII  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'hydra)

(defhydra hydra-drawing (:color pink
                                :pre (overwrite-mode)
                                :post (progn (whitespace-cleanup)
                                             (overwrite-mode -1)))
  "Hydra draw"
  ("f" (hydra-drawing-forward-maintaining-line)                              "forward")
  ("b" backward-char                                                         "backward")
  ("p" (hydra-drawing-move-maintaining-column (lambda () (forward-line -1))) "previous line")
  ("n" (hydra-drawing-move-maintaining-column 'next-line)                    "next line")
  ("q" nil                                                                   "quit" :color blue))
(global-set-key (kbd "C-c C-c d") 'hydra-drawing/body)

(defun hydra-drawing-move-maintaining-column (move-action)
  "Execute `MOVE-ACTION' while attempting to mainting column position."
  (let* ((column-before)
         (column-after))
    (progn
      (setq column-before (current-column))
      (when (= (point) (point-max))
        (newline)
        (backward-char))
      (funcall move-action)
      (setq column-after (current-column))
      (while (< column-after column-before)
        (insert " ")
        (setq column-after (1+ column-after))))))

(defun hydra-drawing-forward-maintaining-line ()
  "Move forward on character attempting to maintain the current line."
  (let* ((line-before (line-number-at-pos)))
    (if (= (point) (point-max))
        (insert " ")
        (progn (forward-char)
               (when (/= line-before (line-number-at-pos))
                 (progn
                   (backward-char)
                   (insert " ")))))))

(provide 'quiescent/drawing-interface)
;;; drawing-interface ends here
