;;; quiescent/haskell --- Setup Haskell mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Cabal packages to install:
;;  - hslint
;;  - hindent
;;  - happy
;;  - hasktags
;;  - stylish-haskell
;;  - present (didn't work though :/ and needed for :present in terminal)

;; A lot of this comes from:
;; https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md

;;; Code:

(defun quiescent-turn-on-haskell-doc-mode ()
  "Activate `haskell-doc-mode'."
  (when (null quiescent-starting-up)
    (turn-on-haskell-doc-mode)))

(use-package haskell-mode
  :straight t
  :config (add-hook 'haskell-mode-hook #'quiescent-turn-on-haskell-doc-mode))

(defun quiescent-activate-dante ()
  "Activate `dante-mode'."
  (when (null quiescent-starting-up)
    (dante-mode 1)))

(use-package dante
  :straight t
  :after haskell-mode
  :commands 'dante-mode
  :init (add-hook 'haskell-mode-hook #'quiescent-activate-dante))

(custom-set-variables '(haskell-tags-on-save t))

(defun quiescent-disable-flycheck-mode ()
  "Disable flycheck mode."
  (when (null quiescent-starting-up)
    (flycheck-mode -1)))

(add-hook 'haskell-debug-mode-hook #'quiescent-disable-flycheck-mode)
(add-hook 'haskell-debug-mode-hook #'quiescent-disable-flyspell-mode)

(defun quiescent-xref-backup-advice (f)
  "Use `xref-find-definitions' as a backup to `interro-goto-definition'.

Used as Advice around the `interro-goto-definition' function F."
  (when (string-equal "Couldn't resolve to any modules." (funcall-interactively f))
    (funcall-interactively #'xref-find-definitions (xref--read-identifier "Find definitions of: "))))

(advice-add #'dante-goto-definition :around #'quiescent-xref-backup-advice)

;; Haskell mode
(use-package async
  :straight t)
(defun haskell-mode-generate-tags (&optional and-then-find-this-tag)
  "Generate tags using Hasktags.  This is synchronous function.

If optional AND-THEN-FIND-THIS-TAG argument is present it is used
with function `xref-find-definitions' after new table was
generated."
  (interactive)
  (let* ((dir (haskell-cabal--find-tags-dir))
         (command (haskell-cabal--compose-hasktags-command dir)))
    (if (not command)
        (error "Unable to compose hasktags command")
      (async-start-process "hasktags" "*hask-tags-buffer*" (split-string command)))))

(provide 'quiescent/haskell)
;;; haskell ends here
