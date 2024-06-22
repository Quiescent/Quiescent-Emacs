;;; custom --- custom variables, faces etc. from the emacs custom system -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(alert-default-style 'fringe-message)
 '(auto-revert-verbose nil)
 '(auto-save-no-message t)
 '(avy-keys '(49 50 51 52 53 54 55 56 57 48))
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(completion-cycle-threshold 100)
 '(completion-styles '(prescient))
 '(completions-sort 'prescient-completion-sort)
 '(confirm-nonexistent-file-or-buffer nil)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eglot-events-buffer-config '(:size 2000000 :format short))
 '(eldoc-echo-area-use-multiline-p nil)
 '(enable-recursive-minibuffers t)
 '(eshell-history-size 1024)
 '(font-lock-maximum-decoration nil)
 '(global-subword-mode t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type 'stack-ghci)
 '(haskell-tags-on-save t)
 '(icomplete-compute-delay 0)
 '(icomplete-delay-completions-threshold 400)
 '(icomplete-max-delay-chars 0)
 '(icomplete-show-matches-on-no-input t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message t)
 '(inhibit-startup-screen t)
 '(jka-compr-verbose nil)
 '(large-file-warning-threshold 50000000)
 '(menu-bar-mode nil)
 '(minibuffer-auto-raise nil)
 '(minibuffer-depth-indicate-mode t)
 '(nano-font-family-monospaced "FiraCode")
 '(nano-font-size 14)
 '(org-agenda-use-time-grid nil)
 '(org-return-follows-link t)
 '(process-error-pause-time 0)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(safe-local-variable-values
   '((outline-minor-mode . t)
     (css-indent-level . 2)
     (svelte-basic-offset . 4)
     (js2-missing-semi-one-line-override)
     (js2-strict-missing-semi-warning)
     (eval progn
           (dolist
               (v
                '("node_modules" "bower_components" ".sass_cache" ".cache" ".npm"))
             (add-to-list 'grep-find-ignored-directories v))
           (dolist
               (v
                '("*.min.js" "*.bundle.js" "*.min.css" "*.json" "*.log"))
             (add-to-list 'grep-find-ignored-files v)))))
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tags-revert-without-query t)
 '(tool-bar-mode nil)
 '(use-dialog-box nil)
 '(use-file-dialog nil)
 '(visible-bell nil)
 '(warning-suppress-log-types '((iedit)))
 '(warning-suppress-types '((emacs)))
 '(zoneinfo-style-world-list
   '(("America/Los_Angeles" "Seattle")
     ("America/New_York" "New York")
     ("Europe/London" "London")
     ("Europe/Paris" "Paris")
     ("Africa/Johannesburg" "Johannesburg"))))

;; Rainbow delimeters theme from: https://ericscrivner.me/2015/06/better-emacs-rainbow-delimiters-color-scheme/
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(error ((t (:inherit nano-face-critical :foreground "gray22" :box (:line-width (2 . 2) :color "#EBCB8B")))))
 '(js2-external-variable ((t (:foreground "orange2" :weight bold))))
 '(js2-highlight-vars-face ((t (:underline t))))
 '(js2-highlight-vars-second-face ((t (:underline "white"))))
 '(slime-highlight-edits-face ((t (:underline "light gray"))))
 '(visible-mark-face1 ((t (:background "slate gray")))))

(provide 'custom)
;;; custom ends here
