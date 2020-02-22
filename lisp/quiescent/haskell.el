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

(require 'use-package)

(defun quiescent-turn-on-haskell-doc-mode ()
  "Activate `haskell-doc-mode'."
  (when (null quiescent-starting-up)
    (turn-on-haskell-doc-mode)))

(use-package haskell-mode
    :ensure t
    :config (add-hook 'haskell-mode-hook #'quiescent-turn-on-haskell-doc-mode))

(defun quiescent-activate-intero ()
  "Activate `intero-mode'."
  (when (null quiescent-starting-up)
    (intero-mode 1)))

(use-package intero
    :ensure t
    :config (add-hook 'haskell-mode-hook #'quiescent-activate-intero))

(custom-set-variables '(haskell-tags-on-save t))

(defun quiescent-disable-flycheck-mode ()
  "Disable flycheck mode."
  (when (null quiescent-starting-up)
    (flycheck-mode -1)))

(add-hook 'haskell-debug-mode-hook #'quiescent-disable-flycheck-mode)
(add-hook 'haskell-debug-mode-hook #'quiescent-disable-flyspell-mode)

;; Use xref as backup when interro can't find the definition
(defun quiescent-xref-backup-advice (f)
  "Use `xref-find-definitions' as a backup to `interro-goto-definition'.

Used as Advice around the `interro-goto-definition' function F."
  (when (string-equal "Couldn't resolve to any modules." (funcall-interactively f))
    (funcall-interactively #'xref-find-definitions (xref--read-identifier "Find definitions of: "))))

(advice-add #'intero-goto-definition :around #'quiescent-xref-backup-advice)

;; Haskell mode
(use-package async
    :ensure t)
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
