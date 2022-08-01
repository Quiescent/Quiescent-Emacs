;;; custom --- custom variables, faces etc. from the emacs custom system -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex")
 '(alert-default-style 'fringe-message)
 '(auto-save-no-message t)
 '(avy-keys '(49 50 51 52 53 54 55 56 57 48))
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(completion-styles '(flex basic partial-completion emacs22))
 '(demo-it--insert-text-speed :fast)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(enable-recursive-minibuffers t)
 '(eshell-history-size 1024)
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
 '(jka-compr-verbose nil)
 '(large-file-warning-threshold 50000000)
 '(max-specpdl-size 32000)
 '(menu-bar-mode nil)
 '(minibuffer-auto-raise nil)
 '(minibuffer-depth-indicate-mode t)
 '(org-pomodoro-clock-break t)
 '(org-pomodoro-play-sounds nil)
 '(org-pomodoro-time-format "")
 '(org-src-window-setup 'current-window)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(safe-local-variable-values
   '((css-indent-level . 2)
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
 '(scroll-bar-mode nil)
 '(send-mail-function 'smtpmail-send-it)
 '(show-paren-mode t)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25)
 '(tags-revert-without-query t)
 '(tide-imenu-flatten t)
 '(tool-bar-mode nil)
 '(warning-suppress-log-types '((iedit)))
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
 '(js2-external-variable ((t (:foreground "orange2"))))
 '(js2-highlight-vars-face ((t (:underline t))))
 '(js2-highlight-vars-second-face ((t (:underline "white"))))
 '(org-pomodoro-mode-line ((t (:foreground "#ff6c6b"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "light steel blue"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "cornflower blue"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "pale turquoise"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "turquoise"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "dodger blue"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "royal blue"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "dark slate blue"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "navy"))))
 '(tide-hl-identifier-face ((t (:inherit nil :underline t))))
 '(visible-mark-face1 ((t (:background "slate gray")))))

(provide 'custom)
;;; custom ends here
