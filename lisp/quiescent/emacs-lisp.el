;;; emacs-lisp --- Convenience, modes and config for editing Emacs Lisp -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'use-package)

(use-package eros
  :ensure t)

(define-key emacs-lisp-mode-map (kbd "C-c C-z") #'quiescent-switch-to-ielm-buffer)

(defun quiescent-switch-to-ielm-buffer ()
  "Switch to ielm buffer.

Creates an ielm buffer if one doesn't exist yet."
  (interactive)
  (let ((open-window (cl-loop
                      for window being the windows
                      when (equal "*ielm*" (buffer-name (window-buffer window)))
                      return window))
        (open-buffer (cl-loop
                      for buffer being the buffers
                      when (equal "*ielm*" (buffer-name buffer))
                      return buffer)))
    (cond
     (open-window (select-window open-window))
     (open-buffer (switch-to-buffer-other-window open-buffer))
     (t           (switch-to-buffer-other-window
                   (save-window-excursion
                     (call-interactively #'ielm)
                     (current-buffer)))))))

(defun quiescent-remove-flex ()
  "Remove the `flex' completion style from completion styles."
  (setq-local completion-styles '(basic partial-completion emacs22)))

(add-hook 'emacs-lisp-mode-hook #'quiescent-remove-flex)

(add-hook 'emacs-lisp-mode-hook #'eros-mode)

(defvar quiescent-edebug-previous-value nil
  "The previous value from evaling the last sexp.")

(defun quiescent-store-last-edebug-value (orig-fun prev-val)
  "Execute ORIG-FUN which should be `edebug-compute-previous-result'.

Store PREV-VAL in variable."
  (progn
    (setq quiescent-edebug-previous-value (edebug-unwrap* prev-val))
    (funcall orig-fun prev-val)))

(advice-add #'edebug-compute-previous-result :around #'quiescent-store-last-edebug-value)

(setq eros-overlays-use-font-lock t)

(defun quiescent-font-lock-by-mode (major-mode)
  "Font lock the current buffer by MAJOR-MODE."
  (delay-mode-hooks (funcall major-mode))
  (font-lock-default-function 'major-mode)
  (font-lock-default-fontify-region (point-min)
                                    (point-max)
                                    nil))

(defun quiescent-edebug-add-eros ()
  "Make edebug print eros statements when debugging."
  (when quiescent-edebug-previous-value
    (eros--make-result-overlay (with-temp-buffer
                                 (insert (format "%s" quiescent-edebug-previous-value))
                                 (quiescent-font-lock-by-mode 'emacs-lisp-mode)
                                 (buffer-string))
      :where (point)
      :duration eros-eval-result-duration)))

(advice-add #'edebug-previous-result :after #'quiescent-edebug-add-eros)

;; Inspired by:
;; http://superuser.com/questions/669701/emacs-disable-some-minibuffer-messages
(defmacro quiescent-suppress-eval-output (&rest body)
  "Supress the output of eval around BODY."
  `(cl-flet ((silence (fun value &optional eval-last-sexp-arg-internal) value))
     (unwind-protect
         (progn
           (advice-add #'elisp--eval-last-sexp-print-value :around #'silence)
           ,(car body))
       (advice-remove #'elisp--eval-last-sexp-print-value #'silence))))

(defun quiescent-el-live-eval ()
  "Evaluate the whole buffer and add overlays to each form."
  (when (eq major-mode 'emacs-lisp-mode)
    (save-excursion
      (goto-char (point-min))
      (let ((message-log-max nil))
        (quiescent-suppress-eval-output
         (while (quiescent-forward-sexp-w/o-error)
           (ignore-errors (eros-eval-last-sexp nil)))
         (ignore-errors (eros-eval-last-sexp nil)))))))

(provide 'quiescent/emacs-lisp)
;;; emacs-lisp ends here
