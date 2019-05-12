;;; quiescent/avy --- Avy configuration and convenience functions  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'use-package)

(use-package avy
    :ensure t
    :config (progn
              (setq avy-timeout-seconds 0.1)
              (global-set-key (kbd "s-x") #'quiescent-avy-super-jump)
              (global-set-key (kbd "s-.") #'avy-goto-word-or-subword-1)
              (global-set-key (kbd "s-c") #'quiescent-avy-copy-symbol)
              (global-set-key (kbd "s-C") #'quiescent-avy-copy-sexp)))

(defun quiescent-avy-super-jump ()
  "Jump to a point with avy and then find the definition."
  (interactive)
  (progn
    (avy-goto-word-or-subword-1)
    (xref-find-definitions (thing-at-point 'symbol))))

(defun quiescent-avy-copy-symbol ()
  "Copy a word subword or symbol using avy."
  (interactive)
  (save-excursion
    (save-window-excursion
      (avy-goto-word-or-subword-1)
      (mark-sexp)
      (kill-ring-save (region-beginning) (region-end))))
  (yank))

(defun quiescent-avy-copy-sexp ()
  "Copy an sexp using avy.
Goes backward up list and then copies the sexp."
  (interactive)
  (save-excursion
    (save-window-excursion
      (avy-goto-word-or-subword-1)
      (backward-up-list)
      (mark-sexp)
      (kill-ring-save (region-beginning) (region-end))))
  (yank))

(defun quiescent-goto-char (&optional str)
  "Perform an isearch-like session with STR.
If candidates are numbered as you go and if you type the number
of a candidate then you can jump to it."
  (interactive)
  (let ((char (read-string (concat "search: " (or str "")))))
    (if (member (aref char 0) '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
        (quiescent-jump-to-candidate char str)
        (let ((search-string (concat str char)))
          (quiescent-highlight-matches search-string)
          (call-interactively (quiescent-goto-char search-string))))))


(define-key isearch-mode-map (kbd "C-'") 'avy-isearch)

(provide 'quiescent/avy)
;;; avy ends here
