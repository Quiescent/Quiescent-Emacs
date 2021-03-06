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
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(auto-save-no-message t)
 '(avy-keys '(49 50 51 52 53 54 55 56 57 48))
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(completion-styles '(flex basic partial-completion emacs22))
 '(demo-it--insert-text-speed :fast)
 '(ecb-options-version "2.50")
 '(ede-project-directories t)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(enable-recursive-minibuffers t)
 '(eshell-history-size 1024)
 '(global-subword-mode t)
 '(haskell-tags-on-save t)
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-ff-search-library-in-sexp t)
 '(helm-grep-input-idle-delay 0.02)
 '(helm-input-idle-delay 0)
 '(helm-mode t)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-scroll-amount 8)
 '(helm-split-window-inside-p t)
 '(icomplete-compute-delay 0)
 '(icomplete-delay-completions-threshold 400)
 '(icomplete-max-delay-chars 0)
 '(icomplete-minibuffer-setup-hook nil)
 '(icomplete-show-matches-on-no-input t)
 '(jka-compr-verbose nil)
 '(large-file-warning-threshold 50000000)
 '(magit-tag-arguments nil)
 '(mail-source-delete-incoming 10)
 '(max-specpdl-size 32000)
 '(menu-bar-mode nil)
 '(minibuffer-auto-raise nil)
 '(minibuffer-depth-indicate-mode t)
 '(org-pomodoro-play-sounds nil)
 '(org-src-window-setup 'current-window)
 '(package-selected-packages
   '(posframe mmm-mode emmet-mode org-pomodoro excorporate ob-mermaid terraform-mode scala-mode rustic nodejs-repl perfect-margin xmlgen polymode org-brain dante ob-async ess robe enh-ruby-mode enh-ruby secretaria racket-mode company-web eacl prism org jupyter emacs-jupyter separedit pdf-tools bufler cl-font-lock flycheck-rust slime-company kotlin-mode lsp-scala company-lsp lsp-ui lsp-mode diredfl diredfl-global-mode all-the-icons-dired docker-tramp ggtags use-package dockerfile org-re-reveal expand-region sql-indent ensime realgud ivy cobol-mode key-chords doom-modeline writeroom-mode centered-cursor-mode json-mode avy doom-themes visible-mark intero flymake-haskell-multi flymake-hlint ereader zerodark-theme bbdb zenburn-theme esh-autosuggest company-ghc js2-highlight-vars paredit sass-mode ob-mongo restclient editorconfig wgrep-ag wgrep crontab-mode tide xref-js2 rjsx-mode esup ace-jump-mode company-statistics company-tern company-mode hindent diff-hl ag emr org-tree-slide demo-it slack 4clojure org-beautify-theme org-bullets org-plus-contrib nlinum exec-path-from-shell sbt-mode projectile zerodark make-it-so smex recentf-ext indium dot-mode julia-mode tern jade js2-refactor jade-mode git-timemachine eros helm cider slime epresent pt kanban plantuml-mode ox-reveal htmlize flx-ido flx typescript-mode yasnippet yaml-mode window-jump web-mode vimgolf use-package-chords typing trie tiny thingatpt+ synonyms swiper swap-regions sr-speedbar smartscan smartparens smart-mode-line rust-mode rainbow-delimiters pp+ powershell page-break-lines ob-http neotree multiple-cursors markdown-mode magit macrostep langtool lacarte keyfreq js2-mode javap-mode javadoc-lookup jammer iy-go-to-char info+ iedit icomplete+ ibuffer-projectile hydra haskell-mode groovy-mode graphviz-dot-mode goto-chg god-mode gnuplot fuzzy-match fsharp-mode frame-cmds fireplace firefox-controller feature-mode fancy-narrow ecb doremi-frm dockerfile-mode docker dired-subtree dired-filter csv-mode crosshairs composable chess bookmark+ auto-highlight-symbol auctex apropos-fn+var aggressive-indent ace-window))
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(safe-local-variable-values
   '((js2-missing-semi-one-line-override)
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
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "unknown" :family "Hack"))))
 '(col-highlight ((t (:background "dim gray"))))
 '(eros-result-overlay-face ((t (:slant italic))))
 '(font-lock-function-name-face ((t (:foreground "#c678dd" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "#51afef" :weight extra-bold))))
 '(hl-line ((t (:inherit nil :background "#2c323b"))))
 '(js2-external-variable ((t (:foreground "orange2"))))
 '(js2-highlight-vars-face ((t (:underline t))))
 '(js2-highlight-vars-second-face ((t (:underline "white"))))
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
