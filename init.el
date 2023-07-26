;;; .emacs --- My emacs root config file.
;;; -*- lexical-binding: t -*-

;;; Commentary:
;; This file is now pointing at a literate program version of
;; my Emacs config in ~/.emacs.d/startup.org
;;
;; Important extra files which are required to be in ~/.emacs.d/
;;  - conf/system-vars.el
;;    Defines a set of variables specific to the system which this
;;    config is being run on (such as which OS is running.)
;;  - org-settings/org-agenda-file-list.el
;;    Sets the list of files which org should use for agenda on
;;    this computer.
;;
;; There's an important and wierd fix which I have to make for OS X.
;; I need to define `x-max-tooltip-size'.  I'm still not really sure
;; why, but I think that 50 should be fine.

;;; Code:

;; For debugging what gets compiled at startup
;;(debug-on-entry #'byte-compile)

;; Don't native compile auto loads.  There's something wrong with them
;; right now: 17/02/2021.
(setq comp-deferred-compilation-deny-list '("\\(?:[^z-a]*-autoloads\\.el$\\)"))

;; Bootstrap straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Early in the file to avoid version conflicts
(straight-use-package 'org)

(defalias 'compat-assoc-delete-all #'assoc-delete-all)
(defalias 'compat-executable-find #'executable-find)
(defalias 'compat-dired-get-marked-files #'dired-get-marked-files)

;; Load system specific configuration
(add-to-list 'load-path "~/.emacs.d/conf")
(require 'system-vars)
(setq quiescent-starting-up t)

(when (file-exists-p "~/.emacs.d/conf/system-conf.el")
  (require 'system-conf))

(use-package key-chord
  :straight t)

(use-package use-package-chords
  :straight t)

(defvar quiescent-toggle-buffer-backwards t
  "Whether we should go forwards or backwards when we next toggle buffers.")

(defun quiescent-toggle-buffer ()
  "Toggle buffers switching to either the previous buffer or the next buffer."
  (interactive)
  (progn
    (if quiescent-toggle-buffer-backwards
        (switch-to-prev-buffer)
      (switch-to-next-buffer))
    (setq quiescent-toggle-buffer-backwards
          (not quiescent-toggle-buffer-backwards))))

(global-set-key (kbd "s-b") #'quiescent-toggle-buffer)
(global-set-key (kbd "s-z") #'isearchb-activate)
(global-set-key (kbd "s-n") #'switch-to-next-buffer)
(global-set-key (kbd "s-p") #'switch-to-prev-buffer)

(when quiescent-exwm-machine
  (eval 
   `(progn
      (use-package xelb
        :straight t
        :demand t)
      (use-package exwm
        :straight t
        :demand t)

      (defun quiescent-exwm-setup-input-simulation ()
        "Setup my bindings to simulate input in EXWM."
        (setq exwm-input-simulation-keys
              '(
                ;; movement
                ([?\C-b] . [left])
                ([?\M-b] . [C-left])
                ([?\C-f] . [right])
                ([?\M-f] . [C-right])
                ([?\C-p] . [up])
                ([?\C-n] . [down])
                ([?\C-a] . [home])
                ([?\C-e] . [end])
                ([?\M-v] . [prior])
                ([?\C-v] . [next])
                ([?\C-d] . [delete])
                ([?\C-k] . [S-end delete])
                ;; cut/paste.
                ([?\C-w] . [?\C-x])
                ([?\M-w] . [?\C-c])
                ([?\C-y] . [?\C-v])
                ;; search
                ([?\C-s] . [?\C-f]))))

      (defun quiescent-exwm-setup-global-bindings ()
        "Setup global bindings for EXWM mode."
        (setq exwm-input-global-keys
              `(
                ;; Bind "s-r" to exit char-mode and fullscreen mode.
                ([?\s-r] . exwm-reset)
                ;; Bind "s-w" to switch workspace interactively.
                ([?\s-w] . exwm-workspace-switch)
                ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
                ,@(mapcar (lambda (i)
                            `(,(kbd (format "s-%d" i)) .
                              (lambda ()
                                (interactive)
                                (exwm-workspace-switch-create ,i))))
                          (number-sequence 0 9))
                ;; Bind "s-&" to launch applications ('M-&' also works if the output
                ;; buffer does not bother you).
                ([?\s-&] . (lambda (command)
                             (interactive (list (read-shell-command "$ ")))
                             (start-process-shell-command command nil command)))
                ([?\s-*] . (lambda ()
                             (interactive)
                             (start-process-shell-command "*Firefox*" nil "firefox --new-tab --url https://www.duckduckgo.com/")))
                ([?\s-$] . (lambda ()
                             (interactive)
                             (start-process-shell-command "*Screenshot*" nil "import /home/edward/screenshot.png")))
                ;; Bind "s-<f2>" to "slock", a simple X display locker.
                ([s-f2] . (lambda ()
                            (interactive)
                            (start-process "" nil "/usr/bin/slock"))))))

      (defun quiescent-exwm-setup-exwm-keys ()
        "Setup some keys which I use in EXWM."
        (progn
          (define-key exwm-mode-map [?\C-q]     #'exwm-input-send-next-key)
          (define-key exwm-mode-map (kbd "s-c") #'org-capture)
          (exwm-input-set-key (kbd "C-c C-j")   #'exwm-input-grab-keyboard)
          (exwm-input-set-key (kbd "s-b")       #'quiescent-toggle-buffer)
          (exwm-input-set-key (kbd "s-z")       #'isearchb-activate)
          (exwm-input-set-key (kbd "s-n")       #'switch-to-next-buffer)
          (exwm-input-set-key (kbd "s-p")       #'switch-to-prev-buffer)))

      (defun quiescent-exwm-setup-displays-and-start ()
        "Setup settings for multiple displays and fire it up!"
        (if quiescent-exwm-multiple-monitors
            ;; From https://github.com/ch11ng/exwm/wiki
            (progn
              (require 'exwm-randr)
              (when quiescent-home-pc-linux
                (setq exwm-randr-workspace-output-plist
                      '(0 "DisplayPort-1" 1 "DisplayPort-1"
                          2 "HDMI-A-0" 3 "HDMI-A-0"
                          4 "HDMI-A-0" 5 "HDMI-A-0"
                          6 "HDMI-A-0" 7 "HDMI-A-0"))
                (add-hook 'exwm-randr-screen-change-hook
                          (lambda ()
                            (start-process-shell-command
                             "xrandr" nil "xrandr --output HDMI-A-0 --left-of DisplayPort-1 --auto")))
                (start-process-shell-command
                 "xrandr" nil "xrandr --output HDMI-A-0 --left-of DisplayPort-1 --auto"))
              (when quiescent-work-machine
                (setq exwm-randr-workspace-output-plist
                      '(0 "DP-2" 1 "DP-2"
                          2 "DP-1" 3 "DP-1"
                          4 "DP-1" 5 "DP-1"
                          6 "DP-1" 7 "DP-1"))
                (add-hook 'exwm-randr-screen-change-hook
                          (lambda ()
                            (start-process-shell-command
                             "xrandr" nil "xrandr --output DP-1 --left-of DP-2 --auto")))
                (start-process-shell-command
                 "xrandr" nil "xrandr --output DP-1 --left-of DP-2 --auto"))
              (exwm-randr-enable))
          (exwm-enable)))

      (when quiescent-exwm-machine
        (require 'exwm-config)
        (defun exwm-config-ido ()
          "Override the config function for ido to do nothing since I don't use it."
          nil)
        (exwm-config-default)
        (fringe-mode 15)
        (quiescent-exwm-setup-input-simulation)
        (quiescent-exwm-setup-exwm-keys)
        (quiescent-exwm-setup-global-bindings)
        (quiescent-exwm-setup-displays-and-start)))))

(setq epa-pinentry-mode 'loopback)

(add-to-list 'load-path "~/.emacs.d/conf")
(require 'system-vars)
(require 'cl-lib)

(setq quiescent-starting-up t)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; * Themes

;; ** Nano

(use-package mini-frame
  :straight t)

(use-package ts
  :straight t)

(use-package svg-tag-mode
  :straight t)

(use-package smex
  :straight t)

(straight-use-package
 '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))

(require 'nano-layout)
(require 'nano-faces)
(require 'nano-theme)
(require 'nano-theme-light)
(require 'nano-theme-dark)
(if (member "--light" command-line-args)
    (nano-theme-set-light)
  (nano-theme-set-dark))
(call-interactively 'nano-refresh-theme)
(defun quiescent-nano-defaults ()
  "Setup nano defaults the way I like 'em."
  (progn
    (menu-bar-mode -1)
    (setq inhibit-startup-screen t)
    (setq inhibit-startup-message t)
    (setq inhibit-startup-echo-area-message t)
    (setq initial-scratch-message nil)
    (setq initial-buffer-choice nil)
    (setq frame-title-format nil)
    (setq use-file-dialog nil)
    (setq use-dialog-box nil)
    (setq pop-up-windows nil)
    (setq indicate-empty-lines nil)
    (setq cursor-in-non-selected-windows nil)
    (setq font-lock-maximum-decoration nil)
    (setq font-lock-maximum-size nil)
    (setq auto-fill-mode nil)
    (setq fill-column 80)
    (setq confirm-nonexistent-file-or-buffer nil)
    (setq org-return-follows-link t)
    (unless (display-graphic-p)
      (xterm-mouse-mode 1)
      (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
      (global-set-key (kbd "<mouse-5>") 'scroll-up-line))
    (if (fboundp 'scroll-bar-mode) (set-scroll-bar-mode nil))
    (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
    (when (eq system-type 'darwin)
      (setq ns-use-native-fullscreen t
            mac-option-key-is-meta t
            mac-command-key-is-meta nil
            mac-option-modifier 'meta
            mac-use-title-bar nil))
    (defun copy-from-osx ()
      (shell-command-to-string "pbpaste"))
    (defun paste-to-osx (text &optional push)
      (let ((process-connection-type nil))
        (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
          (process-send-string proc text)
          (process-send-eof proc))))
    (when (and (not (display-graphic-p))
               (eq system-type 'darwin))
      (setq interprogram-cut-function 'paste-to-osx)
      (setq interprogram-paste-function 'copy-from-osx))
    (setq-default indent-tabs-mode nil)
    (fset 'yes-or-no-p 'y-or-n-p)
    (setq-default tab-width 4)
    (temp-buffer-resize-mode)
    (setq temp-buffer-max-height 8)
    (setq window-min-height 1)
    (prefer-coding-system       'utf-8)
    (set-default-coding-systems 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    (set-language-environment   'utf-8)
    (require 'uniquify)
    (setq uniquify-buffer-name-style 'reverse
          uniquify-separator " • "
          uniquify-after-kill-buffer-p t
          uniquify-ignore-buffers-re "^\\*")
    (unless
        (or (eq system-type 'windows-nt)
            (not (file-exists-p "/bin/bash")))
      (setq-default shell-file-name "/bin/bash")
      (setq explicit-shell-file-name "/bin/bash"))
    (defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
      (if (memq (process-status proc) '(signal exit))
          (let ((buffer (process-buffer proc)))
            ad-do-it
            (kill-buffer buffer))
        ad-do-it))
    (ad-activate 'term-sentinel)
    (setq pop-up-windows t)))
(quiescent-nano-defaults)
(require 'nano-session)
(require 'nano-modeline)
(require 'nano-splash)

;; 

;; ** Prism
(use-package prism
  :straight t
  ;; Based on the readme for prism, adjusted to look closer to the
  ;; original doom theme
  :config (progn
            (defun quiescent-prism-mode ()
              "Enable Prism, only when we're not loading."
              (when (null quiescent-starting-up)
                (prism-mode 1)))
            (prism-set-colors :num 24
              :desaturations (list 10 20 30) :lightens (list 0 -0.25 -5)
              :colors (list 'font-lock-string-face 'font-lock-keyword-face
                            'font-lock-function-name-face 'font-lock-constant-face
                            'font-lock-type-face)))
  :hook (((clojure-mode
           emacs-lisp-mode
           lisp-mode
           racket-mode)
          . quiescent-prism-mode)))
;; 

;; ** Page Break Lines

(use-package page-break-lines
  :straight t
  :config (progn
            (add-to-list 'page-break-lines-modes 'sql-mode)
            (global-page-break-lines-mode 1)))

;; 

;; ** MB Depth

(use-package mb-depth
  :straight t)

;; 

;; ** Crosshair

(use-package xhair
  :straight t)

;; 

;; ** Visual Bell

;; From Phil, posted on https://pragmaticemacs.wordpress.com/2017/10/15/using-a-visible-bell-in-emacs/
;;
;; Date accessed: 17/02/2023
;;
;; The visible bell is usually fine, but still horrid in certain terminals.
;; We can make a nicer version.
(defun quiescent-invert-nano-header ()
  "Invert the nano header line face."
  (progn
    (invert-face 'nano-face-header-default)
    (invert-face 'nano-face-header-strong)))

(defun my-visible-bell ()
  "A friendlier visual bell effect."
  (quiescent-invert-nano-header)
  (run-with-timer 0.1 nil #'quiescent-invert-nano-header))

(define-minor-mode my-visible-bell-mode
   "Use `my-visible-bell’ as the `ring-bell-function’." 
  :global t
  (let ((this 'my-visible-bell-mode))
    (if my-visible-bell-mode
        (progn
          (put this 'visible-bell-backup visible-bell)
          (put this 'ring-bell-function-backup ring-bell-function)
          (setq visible-bell nil
                ring-bell-function #'my-visible-bell))
      ;; Restore the original values when disabling.
      (setq visible-bell (get this 'visible-bell-backup)
            ring-bell-function (get this 'ring-bell-function-backup)))))

(setq visible-bell t)
(my-visible-bell-mode 1)

;; 

;; ** Compilation mode

(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(defun quiescent-enable-ansi-coloured-compilation ()
  "Enabled or disable a hook to colourise compilation buffers."
  (interactive)
  (if (member #'endless/colorize-compilation compilation-filter-hook)
      (remove-hook 'compilation-filter-hook
                   #'endless/colorize-compilation)
    (add-hook 'compilation-filter-hook
              #'endless/colorize-compilation)))

;; 
;; * Editing Anything

;; ** Manipulating Windows and Frames

(defun quiescent-close-help ()
  "Close the help window."
  (interactive)
  (let ((original-window (selected-window)))
    (cl-loop
     for window being the windows
     when (equal "*Help*" (buffer-name (window-buffer window)))
     do (select-window window)
     (call-interactively #'quit-window))
    (select-window original-window)))

(global-set-key (kbd "C-c f") #'make-frame)
(global-set-key (kbd "C-c B") #'browse-url-at-point)
(global-set-key (kbd "s-o")   #'other-frame)
(global-set-key (kbd "s-q")   #'quiescent-close-help)

(key-chord-define-global "x0" #'delete-window)
(key-chord-define-global "x1" #'delete-other-windows)
(key-chord-define-global "x2" #'split-window-below)
(key-chord-define-global "x3" #'split-window-right)

;; 

;; ** Flyspell

(use-package flyspell
  :init (progn
          (setq ispell-dictionary "british")
          (setq ispell-program-name "aspell"))
  :config
  (progn
    (defun quiescent-turn-on-flyspell ()
      "Turn on `flyspell'."
      (when (null quiescent-starting-up)
        (turn-on-flyspell)))
    (define-key flyspell-mode-map (kbd "C-.") nil)
    (add-hook 'text-mode-hook   #'quiescent-turn-on-flyspell)
    (add-hook 'bibtex-mode-hook #'quiescent-turn-on-flyspell)
    (add-hook 'LaTeX-mode-hook  #'quiescent-turn-on-flyspell)
    (add-hook 'TeX-mode-hook    #'quiescent-turn-on-flyspell)))

;; 

;; ** Unfill Paragraph

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Unfill REGION, otherwise paragraph at point.

i.e. the reverse of fill paragraph."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

;; 

;; ** Fill/Unfill All

(defmacro quiescent-for-all-paragraphs (&rest body)
  "Execute BODY with the point in each paragraph in the current buffer."
  `(save-excursion
     (goto-char (point-min))
     (while (not (eq (point) (point-max)))
       (ignore-errors
         ,@body
         (forward-paragraph)))))

(defun quiescent-fill-all-paragraphs ()
  "Run `fill-paragraph' on all paragraphs in the current document."
  (interactive)
  (quiescent-for-all-paragraphs (fill-paragraph)))

(defun quiescent-unfill-all-paragraphs ()
  "Run `unfill-paragraph' on all paragraphs in the current document."
  (interactive)
  (quiescent-for-all-paragraphs (unfill-paragraph)))

;; 

;; ** General Editing Conf

(setq-default indent-tabs-mode nil)
(defun quiescent-disable-indent-tabs-mode ()
  "Disable `indent-tabs-mode'."
  (setq indent-tabs-mode nil))
(add-hook 'prog-mode-hook #'quiescent-disable-indent-tabs-mode)
(setq tab-width 4)
(column-number-mode 1)
(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)

;; 

;; ** Using the Other Window 

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

;; 

;; ** Swapping windows

(defun quiescent-swap-windows ()
  "Swap the two most recently displayed windows."
  (interactive)
  (let* ((windows (window-list (window-frame)))
         (window-1 (car windows))
         (buffer-1 (window-buffer window-1))
         (window-2 (cadr windows))
         (buffer-2 (window-buffer window-2)))
    (when window-2
      (set-window-buffer window-1 buffer-2)
      (set-window-buffer window-2 buffer-1))))

(global-set-key (kbd "s-s") #'quiescent-swap-windows)

;; 

;; ** Treating Buffer Lines as Sets

(defvar *buffer-disjunction-results-buffer-name* "*disjunction-results*"
  "The name of the buffer to display disjunction results in.")

(require 'cl-lib)

(defun buffer-disjunction-lines-from-buffer (buffer)
  "Produce the lines in BUFFER as a list of strings."
  (with-current-buffer buffer
    (split-string (buffer-substring (point-min) (point-max))
                  "\n"
                  " ")))

(defun buffer-disjunction-buffer-disjunction (this-buffer that-buffer)
  "Produce lines which are not in both THIS-BUFFER and THAT-BUFFER.

  Results are displayed in a new buffer controlled by
  `*buffer-disjunction-results-buffer-name*'."
  (interactive "bThis buffer: \nbThat buffer:")
  (let* ((these-lines    (buffer-disjunction-lines-from-buffer this-buffer))
         (those-lines    (buffer-disjunction-lines-from-buffer that-buffer))
         (results-buffer (get-buffer *buffer-disjunction-results-buffer-name*))
         (diff           (mapcar (lambda (x) (concat x "\n"))
                                 (cl-set-difference these-lines those-lines))))
    (switch-to-buffer (if (not (bufferp results-buffer))
                          (generate-new-buffer *buffer-disjunction-results-buffer-name*)
                        results-buffer))
    (erase-buffer)
    (mapc #'insert diff)))

(defvar *buffer-set-difference-results-buffer-name* "*this-but-not-that-results*"
  "The name of the buffer to display this-but-not-that results in.")

(defun buffer-set-difference (this-buffer that-buffer)
  "Produce lines which are in THIS-BUFFER but not THAT-BUFFER.

  Results are displayed in a new buffer controlled by
    `*buffer-set-difference-results-buffer-name*'."
  (interactive "bThis buffer: \nbThat buffer:")
  (let* ((these-lines    (buffer-disjunction-lines-from-buffer this-buffer))
         (those-lines    (mapcar (lambda (x) (cons x t))
                                 (buffer-disjunction-lines-from-buffer that-buffer)))
         (results-buffer (get-buffer *buffer-set-difference-results-buffer-name*))
         (diff           (mapcar (lambda (x) (concat x "\n"))
                                 (cl-remove-if (lambda (x) (assoc x those-lines))
                                               these-lines))))
    (switch-to-buffer (if (not (bufferp results-buffer))
                          (generate-new-buffer *buffer-set-difference-results-buffer-name*)
                        results-buffer))
    (erase-buffer)
    (mapc #'insert diff)))

;; 

;; ** Tiny

(use-package tiny
  :straight t
  :config (tiny-setup-default))

;; 

;; ** Hippie Expand

(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; TODO: this has a bug where the internal state of hippie expand
;; doesn't know that I fiddled with things.

(defvar quiescent-opening-brackets
  (list ?\( ?\[ ?< ?{)
  "Brackets that must be closed during hippie expand.")

(defvar quiescent-closing-brackets
  (list ?\) ?\] ?> ?})
  "Brackets that close an opening bracket during hippie expand.")

(defvar quiescent-start-or-end-of-string
  (list ?\" ?\' ?\`)
  "Characters that constitute the start of a string.")

(require 'cl-lib)

(defun quiescent-closing-bracket-for (char)
  "Produce the closing bracket of CHAR.

Produce nil if CHAR isn't an opening bracket."
  (if (not (member char quiescent-opening-brackets))
      nil
    (nth (cl-position char quiescent-opening-brackets)
         quiescent-closing-brackets)))

(defun quiescent-balance-hippie-insertion (s)
  "Ensure that S is balanced in brackets and string quote chars.

Ignore any syntax other than strings, just push any open brackets
to a stack and ensure they get closed.

At the first char that double closes, end the string, close the
other brackets and produce the result."
  (let ((result)
        (brackets)
        (quotes))
    (let ((final-result (cl-block outer
                          (dotimes (i (length s) (apply #'string (nreverse result)))
                            (let ((char (aref s i)))
                              (cond
                               ((and quotes
                                     (not (member char quiescent-start-or-end-of-string)))
                                (push char result))
                               ((member char quiescent-start-or-end-of-string)
                                (if (eq char (car quotes))
                                    (push (pop quotes) result)
                                  (progn
                                    (push char quotes)
                                    (push char result))))
                               ((member char quiescent-opening-brackets)
                                (progn
                                  (push char brackets)
                                  (push char result)))
                               ((member char quiescent-closing-brackets)
                                (progn
                                  (if (not (eq char (quiescent-closing-bracket-for (car brackets))))
                                      (progn
                                        (while brackets
                                          (push (quiescent-closing-bracket-for (pop brackets)) result))
                                        (cl-return-from outer (apply #'string (nreverse result))))
                                    (progn
                                      (push char result)
                                      (pop brackets)))))
                               (t (push char result))))))))
      (s-trim-right
       (cl-concatenate 'string final-result
                       (nreverse quotes)
                       (mapcar #'quiescent-closing-bracket-for brackets))))))

(defun quiescent-ensure-even-braces-after-hippie (f args)
  "Run F (`hippie-expand') then ensure brackets match.

Pass ARGS to F."
  (let ((start (or he-string-beg (point))))
    (funcall f args)
    (let* ((end (point))
           (inserted (buffer-substring start (point)))
           (balanced (quiescent-balance-hippie-insertion inserted)))
      (when (not (equal inserted balanced))
        (kill-region start (point))
        (insert balanced)
        (setq this-command 'hippie-expand)))))

(advice-add #'hippie-expand :around
            #'quiescent-ensure-even-braces-after-hippie)

(defun quiescent-setup-lisp-string-quotes ()
  "Setup the quotes that open and close strings for lisps."
  (setq-local quiescent-start-or-end-of-string (list ?\")))

(add-hook 'lisp-mode-hook #'quiescent-setup-lisp-string-quotes)
(add-hook 'emacs-lisp-mode-hook #'quiescent-setup-lisp-string-quotes)

;; 

;; ** Avy

(use-package avy
  :straight t
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

(defun quiescent-push-mark-to-ring (&optional arg escape-string no-syntax-crossing)
  "Double `push-mark' for pushing straight onto the ring.
Ignore ARG, ESCAPE-STRING and NO-SYNTAX-CROSSING, they're there
to make the advice work."
  (ignore arg)
  (ignore escape-string)
  (ignore no-syntax-crossing)
  (progn
    (push-mark nil t)
    (push-mark nil t)))

;; 

;; ** Better Backward Up List

(defun quiescent-backward-up-list ()
  "Go `backward-up-list' leaving behind the marker."
  (interactive)
  (progn
    (quiescent-push-mark-to-ring)
    (call-interactively #'backward-up-list)))

(global-set-key (kbd "C-M-u") #'quiescent-backward-up-list)

(with-eval-after-load "paredit"
  (define-key paredit-mode-map (kbd "C-M-u") #'quiescent-backward-up-list))

(advice-add #'nxml-backward-up-element :before
            #'quiescent-push-mark-to-ring)

;; 

;; ** Keep Popping the Ring Until the Point Moved

(defun quiescent-keep-popping-til-moved (pop-function &rest args)
  "Keep popping the mark until either the mark ring is empty or the point moved."
  (interactive)
  (progn
    (while (and mark-ring
                (= (point) (marker-position (car mark-ring))))
      (pop mark-ring))
    (apply pop-function args)))

(advice-add #'pop-to-mark-command :around #'quiescent-keep-popping-til-moved)

;; 

;; ** Make the Mark Visible

(use-package visible-mark
  :straight t
  :config (progn
            (global-visible-mark-mode 1)
            (setq visible-mark-max 1)
            (setq visible-mark-faces `(visible-mark-face1 visible-mark-face2))))

;; 

;; ** Hydra

(use-package hydra
  :straight t
  :config
  (progn
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
    (defhydra hydra-explore-code (:pre (global-auto-highlight-symbol-mode t)
                                       :post (global-auto-highlight-symbol-mode -1)
                                       :color amaranth)
      "
      Code Explorer
      ^Move^                    ^Jump^               ^Project^
      ^^^^^^^^---------------------------------------------------------
      _f_: forward  s-exp       _B_: pop   mark      _s_: project grep
      _b_: backward s-exp       _m_: mark  point     _g_: goto    file
      _n_: forward  list        _j_: xref  jump
      _p_: backward list        _P_: prev  file
      _d_: down     list        _x_: super jump
      _u_: up       list        _i_: imenu
    
      _q_: quit
      "
      ("f" forward-sexp                     nil)
      ("b" backward-sexp                    nil)
      ("n" forward-list                     nil)
      ("p" backward-list                    nil)
      ("d" down-list                        nil)
      ("u" up-list                          nil)
      ("P" (pop-global-mark)                nil)
      ("B" (pop-to-mark-command)            nil)
      ("m" (push-mark)                      nil)
      ("j" xref-find-definitions            nil)
      ("s" quiescent-vc-git-grep            nil)
      ("x" quiescent-avy-super-jump         nil)
      ("q" nil                              nil)
      ("g" project-find-file                nil))
    (key-chord-define-global "qc" 'hydra-explore-code/body)
    ))

;; 

;; ** Window Jump

(use-package window-jump
  :straight t
  :init (when (and (boundp 'key-chord-mode)
                   (not key-chord-mode))
          (key-chord-mode 1)
          (bind-chord ",u" #'window-jump-up)
          (bind-chord ",d" #'window-jump-down)
          (bind-chord ",l" #'window-jump-left)
          (bind-chord ",r" #'window-jump-right)))

;; 

;; ** iSearch

(defun quiescent-kill-isearch-match ()
  "Kill the current isearch match string and continue searching."
  (interactive)
  (kill-region isearch-other-end (point))
  (isearch-repeat-forward))

(define-key isearch-mode-map [(control k)] 'quiescent-kill-isearch-match)

(defun quiescent-first-search-hit ()
  "Activate isearch from the start of the buffer using the existing string if it's present."
  (interactive)
  (progn
    (let ((original-isearch-string (when isearch-mode isearch-string)))
      (goto-char (point-min))
      (isearch-forward nil t)
      (when original-isearch-string (isearch-yank-string original-isearch-string)))))

(defun quiescent-last-search-hit ()
  "Activate isearch from the end of the buffer using the existing string if it's present."
  (interactive)
  (progn
    (let ((original-isearch-string (when isearch-mode isearch-string)))
      (goto-char (point-max))
      (isearch-backward nil t)
      (when original-isearch-string (isearch-yank-string original-isearch-string)))))

(defun quiescent-turn-off-key-chord-mode (&rest _)
  "Turn off `key-chord-mode'."
  (key-chord-mode -1))

(defun quiescent-turn-on-key-chord-mode (&rest _)
  "Turn on `key-chord-mode'."
  (key-chord-mode 1))

(advice-add #'isearch-mode :before #'quiescent-turn-off-key-chord-mode)
(advice-add #'isearch-done :after #'quiescent-turn-on-key-chord-mode)

(define-key isearch-mode-map (kbd "M-<") #'quiescent-first-search-hit)
(define-key isearch-mode-map (kbd "M->") #'quiescent-last-search-hit)

(global-set-key (kbd "M-s M-<") #'quiescent-first-search-hit)
(global-set-key (kbd "M-s M->") #'quiescent-last-search-hit)

;; 

;; ** Crosslink Buffers

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

;; 

;; ** Multiple Cursors

(use-package multiple-cursors
  :straight t
  :config
  (progn
    (global-set-key (kbd "s-SPC")   #'mc/edit-lines)
    (global-set-key (kbd "C->")     #'mc/mark-next-like-this)
    (global-set-key (kbd "C-<")     #'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-<") #'mc/mark-all-like-this)
    (global-set-key (kbd "s->")     #'mc/skip-to-next-like-this)
    (global-set-key (kbd "s-<")     #'mc/skip-to-previous-like-this)))

;; 

;; ** iEdit

(use-package iedit
  :straight t
  :config (global-set-key (kbd "M-s ,") #'iedit-mode))

;; 

;; ** Hi-Scroll-Mode
;; (Based on an XKCD ^_^)

(define-minor-mode hiscroll-mode
  "Toggle hiscroll-mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When hiscroll-mode is enabled the mouse wheel will move the
current buffer through time (i.e. undo/redo while you scroll.)"
  :init-value nil
  :lighter " HiS"
  :keymap
  '(([mouse-5] . hiscroll-undo)
    ([mouse-4] . hiscroll-redo))
  :group 'hiscroll)

(defvar hiscroll-undo-direction 'UNDO
  "The direction to go in history of changes for this buffer.")

(defun hiscroll--correct-direction-and-go (desired-direction)
  "Correct the direction of undoing to DESIRED-DIRECTION and then undo."
  (progn
    (when (not (eq hiscroll-undo-direction desired-direction))
      (setq last-command nil)
      (setq hiscroll-undo-direction desired-direction))
    (undo)))

(defun hiscroll-undo ()
  "Reverse the direction of history if necessary and then continue undoing."
  (interactive)
  (hiscroll--correct-direction-and-go 'UNDO))

(defun hiscroll-redo ()
  "Reverse the direction of history if necessary and then continue redoing."
  (interactive)
  (hiscroll--correct-direction-and-go 'REDO))

;; 

;; ** Recursive Editing

(defun quiescent-general-recursive-edit ()
  "Enter a recursive editing session remembering the window config etc."
  (interactive)
  (save-excursion
    (save-window-excursion
      (recursive-edit))))

(global-set-key (kbd "s-R") #'quiescent-general-recursive-edit)

;; 

;; ** Composable Editing

(eval-and-compile
  (add-to-list 'load-path "~/.emacs.d/lisp/composable.el/")
  (require 'composable)
  (require 'composable-mark)
  (progn (composable-mode)
         (composable-mark-mode))
  (unbind-key (kbd "C-M-\\") composable-mode-map))

;; 

;; ** EACL

(use-package eacl
  :straight t
  :config
  (progn
    (global-set-key (kbd "s-<tab>") #'eacl-complete-multiline)
    (global-set-key (kbd "M-s-i")   #'eacl-complete-line)))

;; 

;; ** Fido

(fido-vertical-mode t)

(use-package prescient
  :straight t)

;; 

;; ** Q-Complete
;;
;; This is my attempt at the completion style that I've always wanted
;; and company only gets close to.

(defun q-complete-or-indent ()
  "If there are completions, `q-complete' otherwise indent."
  (interactive)
  (if (and (not (region-active-p))
           (q-complete--all-completions))
      (q-complete)
    (funcall-interactively #'indent-for-tab-command)))

(defvar q-complete-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "<tab>") #'q-complete-or-indent)
    keymap)
  "Keymap for `q-complete'.")

(define-minor-mode q-complete-mode ()
  "Unintrusive, inline completion."
  :init-value nil
  :lighter nil
  :global nil)

(add-hook 'prog-mode-hook #'q-complete-mode)

(defvar q-complete-saved-cursor nil
  "The style of cursor before we meddled with it.")

(defun q-complete--restore-cursor ()
  "If there's a saved cursor, restore it."
  (when q-complete-saved-cursor
    (setq cursor-type q-complete-saved-cursor
          q-complete-saved-cursor nil)))

(defun q-complete--set-cursor ()
  "Set the cursor to a bar to indicate we're completing."
  (when (null q-complete-saved-cursor)
    (setq q-complete-saved-cursor cursor-type
          cursor-type (cons 'hbar 2))))

(defun q-complete-transient-quit ()
  "Exit transient completion when the user does anything."
  (interactive)
  (progn
    (setq q-complete-original-start nil
          q-complete-transient-original-text nil
          q-complete-transient-candidates nil
          q-complete-transient-start-point nil
          q-complete-transient-end-point nil
          q-complete-transient-candidate-index nil)
    (q-complete-transient-mode -1)
    (q-complete-mode 1)))

(defvar q-complete-recent-completions nil
  "A list of the completions that were recently selected.")

(defun q-complete-remember (hit)
  "Add HIT to the front of the list of recent completions."
  (setq q-complete-recent-completions
        (cons hit (cl-remove hit q-complete-recent-completions :test #'string-equal))))

(defun q-complete--tide-completions (prefix)
  (let ((file-location
         `(:file ,(tide-buffer-file-name)
                 :line ,(tide-line-number-at-pos) :offset ,(- (tide-current-offset) (length prefix))
                 :includeExternalModuleExports ,tide-completion-enable-autoimport-suggestions
                 :includeInsertTextCompletions t)))
    (when (and (not (tide-in-string-p)) (not (tide-member-completion-p prefix)))
      (setq file-location (-concat file-location `(:prefix ,prefix))))
    (prescient-sort (mapcar (lambda (hit) (plist-get hit :name))
                            (plist-get (tide-send-command-sync
                                        "completions"
                                        file-location)
                                       :body)))))

(defun q-complete-transient-insert-and-quit ()
  "Insert the last key that was pressed and exit `q-complete-transient-mode'."
  (interactive)
  (progn
    ;; Use prescient to remember that we completed to this one
    (let ((hit (nth q-complete-transient-candidate-index
                    q-complete-transient-candidates)))
      (when hit
        (prescient-remember hit)
        (q-complete-remember hit)))
    (setq unread-command-events (list last-input-event))
    (q-complete-transient-quit)))

(defun q-complete-transient-abort-then-mark ()
  "Quit `q-complete-transient-mode' and activate the mark."
  (interactive)
  (progn
    (let ((hit (nth q-complete-transient-candidate-index
                    q-complete-transient-candidates)))
      (when hit
        (prescient-remember hit)
        (q-complete-remember hit)))
    (q-complete-transient-quit)
    (call-interactively #'set-mark-command)))

(defun q-complete-transient-abort-then-mark-sexp ()
  "Quit `q-complete-transient-mode' and mark next sexp."
  (interactive)
  (progn
    (let ((hit (nth q-complete-transient-candidate-index
                    q-complete-transient-candidates)))
      (when hit
        (prescient-remember hit)
        (q-complete-remember hit)))
    (q-complete-transient-quit)
    (call-interactively #'mark-sexp)))

(defvar q-complete-transient-mode-map
  (let ((map (make-keymap)))
    (set-char-table-range (nth 1 map) t #'q-complete-transient-insert-and-quit)
    (define-key map (kbd "ESC") nil)
    (define-key map (kbd "C-SPC") #'q-complete-transient-abort-then-mark)
    (define-key map (kbd "C-M-SPC") #'q-complete-transient-abort-then-mark-sexp)
    (define-key map (kbd "C-M-f") #'q-complete-transient-insert-and-quit)
    (define-key map (kbd "C-M-b") #'q-complete-transient-insert-and-quit)
    (define-key map (kbd "C-M-u") #'q-complete-transient-insert-and-quit)
    (define-key map (kbd "C-M-d") #'q-complete-transient-insert-and-quit)
    (define-key map (kbd "q") #'q-complete-transient-abort)
    (define-key map (kbd "<tab>") #'q-complete-transient-next-candidate)
    (define-key map (kbd "<backtab>") #'q-complete-transient-previous-candidate)
    (define-key map (kbd "C-s") #'q-complete-isearch)
    (define-key map (kbd "C-r") #'q-complete-isearch)
    map)
  "Keymap used by `q-complete-transient-mode'.")

(define-minor-mode q-complete-transient-mode ()
  "A mode that's invoked during completion to scroll through options."
  :init-value nil
  :ligther nil
  :global nil
  (if q-complete-transient-mode
      (q-complete--set-cursor)
    (q-complete--restore-cursor)))

(defvar q-complete-transient-original-text nil
  "The text that was originally to be completed.")

(defvar q-complete-original-start nil
  "The position we invoked `q-complete' from.")

(defvar q-complete-transient-candidates nil
  "A list of completion candidates for this transient session.")

(defvar q-complete-transient-start-point nil
  "The position in the buffer where the thing we're completing starts.")

(defvar q-complete-transient-end-point nil
  "The position in the buffer where the thing we're completing ends.")

(defvar q-complete-transient-candidate-index nil
  "The index of the currently selected candidate.")

(require 'prescient)

(prescient-persist-mode 1)

(defun q-complete--all-completions-tide ()
  "Produce a table of all the completions at point."
  (let* ((thing-spec 'symbol)
         (text   (or (thing-at-point thing-spec) ""))
         (bounds (or (bounds-of-thing-at-point thing-spec)
                     (cons (point) (point))))
         (text-start (car bounds))
         (text-end   (cdr bounds)))
    (let* ((all-completions (q-complete--tide-completions text)))
      (when (not (null all-completions))
        (list all-completions text-start text-end text)))))

(defun q-complete--all-completions-tern (&optional dont-repeat)
  "Produce the completions at point using tern.

If DONT-REPEAT is non-nil, don't try the function and recurse."
  (pcase (tern-completion-at-point)
    (`(,text-start ,text-end ,hits)
     (list (prescient-sort hits)
           text-start
           text-end
           (buffer-substring text-start text-end)))
    (t
     (when (null dont-repeat)
       (let ((window-config (current-window-configuration)))
        (funcall (tern-completion-at-point))
        (sleep-for 0 50)
        (q-complete--all-completions-tern t)
        (set-window-configuration window-config))))))

(defun q-complete--all-completions ()
  "Produce a table of all the completions at point."
  (cond
   ((and (boundp 'tide-mode) tide-mode) (q-complete--all-completions-tide))
   ((and (boundp 'tern-mode) tern-mode) (q-complete--all-completions-tern))
   (t (let* ((thing-spec 'symbol)
             (text   (or (thing-at-point thing-spec) ""))
             (bounds (or (bounds-of-thing-at-point thing-spec)
                         (cons (point) (point))))
             (text-start (car bounds))
             (text-end   (cdr bounds)))
        (let* ((pos (cl-search (buffer-substring text-start (point)) text))
               ;; From minibuffer.el
               (capf-results (run-hook-wrapped 'completion-at-point-functions
                                               #'completion--capf-wrapper 'all))
               (all-completions (completion-all-completions
                                 text
                                 (thread-last (nth 3 capf-results)
                                              (prescient-filter text)
                                              prescient-completion-sort)
                                 #'identity
                                 pos)))
          (when (not (null (nth 0 all-completions)))
            (list all-completions text-start text-end text)))))))

(defun q-complete ()
  "Enter `q-complete-transient-mode' for the text around point."
  (interactive)
  (pcase (q-complete--all-completions)
    (`(,all-completions ,text-start ,text-end ,text)
     (when (not (null (nth 0 all-completions)))
       (q-complete-mode -1)
       (q-complete-transient-mode t)
       (setq q-complete-original-start (point)
             q-complete-transient-original-text text
             q-complete-transient-candidates all-completions
             q-complete-transient-start-point text-start
             q-complete-transient-end-point text-end
             ;; We currently don't have a completion selected
             q-complete-transient-candidate-index -1)
       ;; Select the first completion
       (q-complete-transient-next-candidate)))))

(defun q-complete-transient-abort ()
  "Restore the original string and position from before completion and quit."
  (interactive)
  (progn
    (when (and q-complete-transient-start-point q-complete-transient-end-point)
      (kill-region q-complete-transient-start-point
                  q-complete-transient-end-point))
    (when (and q-complete-transient-start-point q-complete-transient-original-text)
      (goto-char q-complete-transient-start-point)
      (insert q-complete-transient-original-text))
    (q-complete-transient-quit)))

(defun q-complete--highlight-prefix ()
  "Highilght the part of the completion that matches the prefix."
  )

(defun q-complete--insert-completion ()
  "Insert the current completion at point and update trackers."
  (let ((post-command-hook nil))
    (kill-region q-complete-transient-start-point
                 q-complete-transient-end-point)
    (goto-char q-complete-transient-start-point)
    (let ((candidate (nth q-complete-transient-candidate-index
                          q-complete-transient-candidates)))
      (setq q-complete-transient-end-point
            (+ q-complete-transient-start-point
               (length candidate)))
      (insert candidate))))

(defun q-complete-isearch ()
  "Activate `q-complete-isearch-mode'."
  (interactive)
  (q-complete-isearch-mode t)
  (q-complete-isearch-message))

(defvar q-complete-isearch-text nil
  "The string that we're searching completions for.")

(defun q-complete-isearch-message ()
  "Display what we're searching for."
  (message "%s" (concat (apply #'propertize
                               "Search: "
                               minibuffer-prompt-properties)
                        q-complete-isearch-text)))

(defun q-complete-isearch-insert ()
  "Insert the last character to the end of the search text."
  (interactive)
  (progn
    (setq q-complete-isearch-text
          (concat q-complete-isearch-text (string last-command-event)))
    (if (eq q-complete-isearch-direction 'forward)
        (q-complete-isearch-forward t)
      (q-complete-isearch-backward t))))

(defun q-complete-isearch-backspace ()
  "Backspace the last searched character."
  (interactive)
  (progn
    (setq q-complete-isearch-text
          (substring q-complete-isearch-text
                     0
                     (1- (length q-complete-isearch-text))))
    (if (eq q-complete-isearch-direction 'forward)
        (q-complete-isearch-forward t)
      (q-complete-isearch-backward t))))

(defun q-complete-isearch-abort ()
  "Abort `q-complete-isearch' and drop to transient completion.

Keeps the index where it moved to during the search session."
  (interactive)
  (when q-complete-highlight-search-hit-overlay
    (delete-overlay q-complete-highlight-search-hit-overlay)
    (setq q-complete-highlight-search-hit-overlay nil))
  (q-complete-isearch-mode -1))

(defun q-complete-isearch-pre-command-hook ()
  "Abort exit search and `q-complete' sessions if command should.

e.g. if you are searching and `forward-char' then it should exit
both `q-complete-transient-mode' and `q-complete-isearch-mode'.

See `isearch-pre-command-hook' for inspiration of this approach."
  (cond
   ((member (this-single-command-keys) (list (kbd "<tab>")
                                             [32] ; Space
                                             ))
    (q-complete-isearch-abort))
   ((and (not (eq 'self-insert-command
                  (lookup-key global-map (this-single-command-keys))))
         (not (commandp (lookup-key q-complete-isearch-mode-map
                                    (this-single-command-keys)
                                    nil))))
    (q-complete-isearch-abort))))

(defvar q-complete-isearch-mode-map
  (let ((keymap (make-sparse-keymap)))
    ;; We don't want to include space here because that should
    ;; terminate completion.
    (let ((c (1+ ?\s)))
      (while (< c 256)
        (define-key keymap (vector c) #'q-complete-isearch-insert)
        (cl-incf c)))
    (define-key keymap (kbd "C-s") #'q-complete-isearch-forward)
    (define-key keymap (kbd "C-r") #'q-complete-isearch-backward)
    (define-key keymap (kbd "C-g") #'q-complete-isearch-abort)
    (define-key keymap (kbd "<backspace>") #'q-complete-isearch-backspace)
    keymap)
  "Keymap for `q-complete-isearch-mode'.")

(define-minor-mode q-complete-isearch-mode ()
  "A mode that searches through the current q-complete candidates."
  :init-value nil
  :lighter nil
  :global nil
  (progn
    (setq q-complete-isearch-text
          (if q-complete-isearch-mode "" nil))
    (if q-complete-isearch-mode
        (add-hook 'pre-command-hook #'q-complete-isearch-pre-command-hook)
      (remove-hook 'pre-command-hook #'q-complete-isearch-pre-command-hook))))

(defvar q-complete-isearch-direction 'forward
  "The direction to search through candidates.")

(defvar q-complete-highlight-search-hit-overlay nil
  "An overlay that marks the part of the buffer that matches the search term.")

(defun q-complete--highlight-search-hit ()
  "Highlight the part of the completion that matches the search."
  (progn
    (when q-complete-isearch-text
      (let* ((hit-start (cl-search q-complete-isearch-text
                                   (buffer-substring q-complete-transient-start-point
                                                     q-complete-transient-end-point)))
             (start (+ q-complete-transient-start-point
                       (or hit-start 0)))
             (end (+ start (length q-complete-isearch-text))))
        (when hit-start
          (if (null q-complete-highlight-search-hit-overlay)
              (progn
                (setq q-complete-highlight-search-hit-overlay
                      (make-overlay start end))
                (overlay-put q-complete-highlight-search-hit-overlay
                             'face 'highlight))
            (move-overlay q-complete-highlight-search-hit-overlay
                          start
                          end)))))))

(defun q-complete-isearch-forward (&optional no-advance)
  "Search forward from the current index.

Exit `q-complete' on RET.
Drop back to tabbing through hits on `quit'.

If NO-ADVANCE is t supplied then we don't bump the pointer."
  (interactive)
  (progn
    (setq q-complete-isearch-direction 'forward)
    (when (not no-advance)
      (if (not (consp (nthcdr q-complete-transient-candidate-index
                              q-complete-transient-candidates)))
          (setf q-complete-transient-candidate-index 0)
        (cl-incf q-complete-transient-candidate-index)))
    (let ((pointer (nthcdr q-complete-transient-candidate-index
                           q-complete-transient-candidates)))
      (while (and (consp pointer)
                  (not (string-match q-complete-isearch-text
                                     (car pointer))))
        (setq pointer (cdr pointer))
        (cl-incf q-complete-transient-candidate-index))
      (if (not (consp pointer))
          (progn
            (setq q-complete-transient-candidate-index 0)
            (funcall ring-bell-function)
            (message "%s" (concat (apply #'propertize
                                         "Overwrapped search: "
                                         minibuffer-prompt-properties)
                                  q-complete-isearch-text)))
        (progn
          (q-complete--insert-completion)
          (q-complete--highlight-search-hit)
          (q-complete-isearch-message))))))

(defun q-complete--move-to-last-hit ()
  "Move the pointer to the last hit."
  (let ((pointer q-complete-transient-candidates))
    (while (consp pointer)
      (setq pointer (cdr pointer))
      (cl-incf q-complete-transient-candidate-index))
    (cl-decf q-complete-transient-candidate-index)))

(defun q-complete-isearch-backward (&optional no-advance)
  "Search forward from the current index.

Exit `q-complete' on RET.
Drop back to tabbing through hits on `quit'.

If NO-ADVANCE is t supplied then we don't bump the pointer."
  (interactive)
  (progn
    (setq q-complete-isearch-direction 'backward)
    (when (not no-advance)
      (if (= 0 q-complete-transient-candidate-index 0)
          (q-complete--move-to-last-hit)
        (cl-decf q-complete-transient-candidate-index)))
    (while (and (/= 0 q-complete-transient-candidate-index)
                (not (string-match q-complete-isearch-text
                                   (nth q-complete-transient-candidate-index
                                        q-complete-transient-candidates))))
      (cl-decf q-complete-transient-candidate-index))
    (if (= 0 q-complete-transient-candidate-index)
        (progn
          (q-complete--move-to-last-hit)
          (funcall ring-bell-function)
          (message "%s" (concat (apply #'propertize
                                       "Overwrapped search: "
                                       minibuffer-prompt-properties)
                                q-complete-isearch-text)))
      (progn
        (q-complete--insert-completion)
        (q-complete--highlight-search-hit)
        (q-complete-isearch-message)))))

(defun q-complete-transient-next-candidate ()
  "Replace the completion text with the next candidate."
  (interactive)
  (progn
    (setf q-complete-transient-candidate-index
          (1+ q-complete-transient-candidate-index))
    (when (not (consp (nthcdr q-complete-transient-candidate-index
                              q-complete-transient-candidates)))
      (setq q-complete-transient-candidate-index 0))
    (q-complete--insert-completion)))

(defun q-complete-transient-previous-candidate ()
  "Replace the completion text with the next candidate."
  (interactive)
  (progn
    (if (= 0 q-complete-transient-candidate-index)
        (q-complete--move-to-last-hit)
      (cl-decf q-complete-transient-candidate-index))
    (q-complete--insert-completion)))

;; 

;; Compilation Mode

(require 'compile)

(key-chord-define-global "xr" 'recompile)

;; TODO more carefully consider which modes need flycheck
(use-package flycheck
  :straight t
  :init (progn
	      (defun quiescent-enable-flycheck-prog (&optional rest)
	        "Decide whether flycheck should be enabled in this prog mode.

Ignore REST."
	        (when (and (null quiescent-starting-up)
		               (not (member major-mode quiescent-modes-not-to-activate-flycheck-in)))
	          (flycheck-mode 1)))
	      (add-hook 'prog-mode-hook #'quiescent-enable-flycheck-prog))
  :config
  (defun quiescent-enable-flycheck (&optional rest)
    "Enable flycheck mode.

    Ignore REST."
    (when (null quiescent-starting-up)
      (flycheck-mode 1)))

  (defun quiescent-disable-flycheck (&optional rest)
    "Disable flycheck mode.

    Ignore REST."
    (flycheck-mode -1))
  (defvar quiescent-modes-not-to-activate-flycheck-in '(haskell-mode emacs-lisp-mode rust-mode)
    "Modes in which flycheck should not be activated.

Usually because of too much overhead in checking.")
  :hook (((latex-mode message-mode
		              org-mode text-mode
		              gfm-mode markdown-mode LaTeX-mode)
	      . quiescent-enable-flycheck)))

(with-eval-after-load "flycheck"
  (progn
    (define-key flycheck-mode-map (kbd "M-n") #'flycheck-next-error)
    (define-key flycheck-mode-map (kbd "M-p") #'flycheck-previous-error)))

(defun quiescent-add-probable-include-dir-for-cpp ()
  "Add a probable include directory clang flycheck.

  We guess that the include dir is probably one up and into
  `include'."
  (when (null quiescent-starting-up)
    (setq flycheck-clang-include-path
	      (list (expand-file-name "../include/")))))

(defun quiescent-set-flycheck-language-standard ()
  "Set the language standard for flycheck."
  (when (null quiescent-starting-up)
    (setq flycheck-clang-language-standard "c++11")))

(add-hook 'c++-mode-hook
	      #'quiescent-set-flycheck-language-standard)
(add-hook 'c++-mode-hook
	      #'quiescent-add-probable-include-dir-for-cpp)
(add-hook 'c-mode-hook
	      #'quiescent-add-probable-include-dir-for-cpp)

;; 

;; ** Flymake

(require 'flymake)

(define-key flymake-mode-map (kbd "M-p") #'flymake-goto-prev-error)
(define-key flymake-mode-map (kbd "M-n") #'flymake-goto-next-error)

(defun quiescent-enable-flymake-mode ()
  "Enable `flymake-mode' in the current buffer."
  (when (null quiescent-starting-up)
    (flymake-mode 1)))

(add-hook 'emacs-lisp-mode-hook #'flymake-mode)

;; ** Show Parenthises

(use-package paren
  :config (progn
            (setq show-paren-style 'parenthesis)
            (show-paren-mode +1)
            (setq show-paren-when-point-inside-paren t)
            (setq show-paren-when-point-in-periphery t)))

;; 

;; ** Yasnippet

;; From SO: http://emacs.stackexchange.com/questions/12613/convert-the-first-character-to-uppercase-capital-letter-using-yasnippet
(defun kaushalmodi-capitalize-first-char (string)
  "Capitalize only the first character of the input `STRING'."
  (when (and string (> (length string) 0))
    (let ((first-char (substring string 0 1))
          (rest-str   (substring string 1)))
      (concat (capitalize first-char) rest-str))))

(use-package yasnippet
  :straight t
  :init (progn
          (yas-global-mode 1)
          (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")))

;; 

;; ** Paredit

(defun quiescent-activate-paredit-mode ()
  "Activate paredit mode."
  (when (null quiescent-starting-up)
    (enable-paredit-mode)))

(use-package paredit
  :straight t
  :config (progn
            (define-key paredit-mode-map (kbd "C-M-n") #'forward-list)
            (define-key paredit-mode-map (kbd "C-M-p") #'backward-list)
            (define-key paredit-mode-map (kbd "<RET>") nil)
            (add-hook 'emacs-lisp-mode-hook                  #'quiescent-activate-paredit-mode)
            (add-hook 'eval-expression-minibuffer-setup-hook #'quiescent-activate-paredit-mode)
            (add-hook 'ielm-mode-hook                        #'quiescent-activate-paredit-mode)
            (add-hook 'lisp-mode-hook                        #'quiescent-activate-paredit-mode)
            (add-hook 'lisp-interaction-mode-hook            #'quiescent-activate-paredit-mode)
            (add-hook 'scheme-mode-hook                      #'quiescent-activate-paredit-mode)
            (add-hook 'cider-mode-hook                       #'quiescent-activate-paredit-mode)
            (add-hook 'cider-repl-mode-hook                  #'quiescent-activate-paredit-mode)
            (add-hook 'slime-repl-mode-hook                  #'quiescent-activate-paredit-mode)
            (add-hook 'racket-mode-hook                      #'quiescent-activate-paredit-mode)
            (add-hook 'racket-repl-mode-hook                 #'quiescent-activate-paredit-mode)))

;; 

;; ** Electric Pair

(require 'elec-pair)

(defun quiescent-activate-electric-pair ()
  "Activate electric pair local mode."
  (when (null quiescent-starting-up)
    (electric-pair-local-mode)))

(mapc (lambda (mode-hook) (add-hook mode-hook #'quiescent-activate-electric-pair))
      '(js-mode-hook
        js2-mode-hook
        rjsx-mode-hook
        python-mode-hook
        typescript-mode-hook
        sh-mode-hook
        css-mode-hook
        haskell-mode-hook
        eshell-mode-hook
        restclient-mode-hook
        scala-mode-hook
        java-mode-hook
        conf-unix-mode-hook
        c++-mode-hook
        yaml-mode-hook
        ruby-mode-hook
        intero-repl-mode-hook
        makefile-mode-hook
        rust-mode-hook
        rustic-mode-hook
        conf-mode-hook
        inferior-python-mode-hook
        inf-ruby-mode-hook
        enh-ruby-mode-hook
        ess-r-mode-hook
        inferior-ess-r-mode-hook
        terraform-mode-hook
        c-mode-hook
        csharp-mode-hook
        svelte-mode-hook))

(add-to-list 'electric-pair-pairs (cons ?\( ?\)))
(setq-local parse-sexp-ignore-comments t)

;; 

;; ** Save Place

(use-package saveplace
  :defer 10
  :init (save-place-mode 1))

;; 

;; ** Preview Region in Specific Mode

(defun quiescent-point-at-start-of-line ()
  "Produce the point at the start of the line."
  (save-excursion
    (beginning-of-line)
    (point)))

(defun quiescent-point-at-end-of-line ()
  "Produce the point at the end of the line."
  (save-excursion
    (end-of-line)
    (point)))

;; Use eshell after prompt hook here for guessing content
(defun quiescent-format-previous-content ()
  "Try to guess the format of the previous content in eshell and format it."
  (save-excursion
    (progn
      (forward-line)
      (while (not (= (point) (point-max)))
        (when (temp-view-guess-data-type (thing-at-point 'line t))
          (let ((original-buffer  (current-buffer))
                (formatted        (temp-view-region (quiescent-point-at-start-of-line)
                                                    (quiescent-point-at-end-of-line)
                                                    t)))
            (beginning-of-line)
            (kill-line)
            (insert formatted)))
        (forward-line)))))

(defvar *temp-view-produce-output* nil
  "If non-nil then all functions which would create a buffer instead produce the output.")

(defun temp-view-region (beg end &optional produce-output)
  "View the region [BEG, END) from the current in a temp buffer.

Tries to guess the kind of content and setup a veiw of that data
which is appropirate.

If PRODUCE-OUTPUT is non-nil then don't create a buffer, instead
produce the formatted text as output."
  (interactive "r")
  (let ((region           (buffer-substring beg end))
        (*temp-view-produce-output* produce-output))
    (pcase (temp-view-guess-data-type region)
      ('XML  (temp-view-xml region))
      ('HTML (temp-view-html region))
      ('JSON (temp-view-json region)))))

(defvar temp-view-region-match-alist
  '(("\\(<div>\\)\\|\\(<body>\\)\\(<head>\\)" . HTML)
    ("<[a-z]+.*>.*</[a-z]+>" . XML)
    ("{.*\\(\\(\n\\)\\|\\(.*\\)\\).*\\(\\(\"[a-z0-9]+\"\\)\\|\\([a-z0-9]+\\)\\):" . JSON))
  "An alist of regular expressions to match text to a type of text.

Note that ordering is important.  The first match will be the one
which ends up being reflected.")

(defun temp-view-guess-data-type (text)
  "Guess what kind of data TEXT is."
  (let (result)
    (dolist (matcher temp-view-region-match-alist result)
      (when (and (not result)
                 (string-match (car matcher) text))
        (setq result (cdr matcher))))))

(defun temp-view-insert-xml (mode text)
  "Insert TEXT, format xml like data and then activate MODE."
  (progn
    (insert text)
    (goto-char (point-min))
    (while (search-forward "><" nil t nil)
      (replace-match ">\n<"))
    (funcall mode)
    (indent-region (point-min) (point-max))))

(defun temp-window-xml-like (buffer-name mode text)
  "Create a temp buffer named BUFFER-NAME in MODE containing TEXT."
  (if *temp-view-produce-output*
      (with-temp-buffer
        (temp-view-insert-xml mode text)
        (buffer-substring (point-min) (point-max)))
    (prog1
        (switch-to-buffer-other-window
         (generate-new-buffer buffer-name))
      (temp-view-insert-xml mode text))))

(defun temp-view-xml (text)
  "View TEXT in a temp buffer setup for viewing xml."
  (temp-window-xml-like "*temp-view-xml*" #'nxml-mode text))

(defun temp-view-html (text)
  "View TEXT in a temp buffer setup for viewing html."
  (temp-window-xml-like "*temp-view-html*" #'web-mode text))

(defun temp-view-insert-json (text)
  "Insert TEXT into the buffer at point, activate `json-mode' and format the text."
  (progn
    (insert text)
    (goto-char (point-min))
    (json-mode)
    (json-pretty-print (point-min) (point-max))))

(defun temp-view-json (text)
  "View TEXT in a temp buffer setup for viewing json."
  (if *temp-view-produce-output*
      (with-temp-buffer
        (temp-view-insert-json text)
        (buffer-substring (point-min) (point-max)))
    (prog1
        (switch-to-buffer-other-window
         (generate-new-buffer "*temp-view-json*"))
      (temp-view-insert-json text))))

(global-set-key (kbd "s-v") #'temp-view-region)

;; Not working and a bit too slow for now
;; (add-hook 'eshell-after-prompt-hook #'quiescent-format-previous-content)

;; 

;; ** Insert Random Int

(defvar quiescent-random-int-threshold 20000
  "A threshold for the randomly generated integers.")

(defun quiescent-insert-reasonable-random-int-here ()
  "Insert a reasonable random int at point."
  (interactive)
  (insert (format "%s" (mod (abs (random)) quiescent-random-int-threshold))))

;; 

;; ** Editor Conf

(use-package editorconfig
  :straight t
  :init (with-eval-after-load "editorconfig"
          (editorconfig-mode 1)))

;; ** Eros

(use-package eros
  :straight t)

;; 

;; ** Too Long Lines

(use-package too-long-lines-mode
  :load-path "~/.emacs.d/lisp/too-long-lines-mode/"
  :config (too-long-lines-mode 1))

;; 

;; ** Ediff

(require 'ediff)

(defun ediff-copy-both-to-C ()
  "Copy both regions to the result buffer."
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

(defun add-d-to-ediff-mode-map ()
  "Add d to the keymap for ediff -- which copies both to the result buffer."
  (when (null quiescent-starting-up)
    (define-key ediff-mode-map "d" 'ediff-copy-both-to-C)))

(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

;; 

;; ** Recent Files (recentf)

(require 'recentf)

(recentf-mode t)

(progn
  (setq recentf-max-saved-items 50)
  (run-at-time nil (* 5 60) 'recentf-save-list))

(defun quiescent-open-recent-file ()
  "Open a recent file use recentf for completion."
  (interactive)
  (find-file (completing-read "Recent file: " recentf-list)))

(use-package recentf-ext
  :straight t)

(defun quiescent-ignore-regex-case ()
  "Replace the alpha-only word at point with a case insensitive version."
  (interactive)
  (progn
    (search-backward-regexp "[a-zA-Z]*[^a-zA-Z]")
    (forward-char)
    (save-match-data
      (save-excursion
        (search-forward-regexp "\\([a-zA-Z]*\\)[^a-zA-Z]?"))
      (replace-match (mapconcat (lambda (char) (list ?\[ char (upcase char) ?\]))
                                (downcase (match-string 0)) "")))))

(global-set-key (kbd "C-h C-f")   #'recentf-open-files)

;; 

;; ** VC Git Grep

(defun quiescent-vc-git-grep (regexp)
  "Search for REGEXP in the current project using grep.

Optionally you may specify a glob pattern for the typo of file to
search through."
  ;; This comes from the definition of `vc-git-grep'
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((equal current-prefix-arg '(16))
       (list (read-from-minibuffer "Run: " "git grep"
                                   nil nil 'grep-history)
             nil))
      (t (let* ((regexp (grep-read-regexp)))
           (list regexp))))))
  (vc-git-grep regexp "*" (vc-git-root default-directory)))

(global-set-key (kbd "C-c p s g") #'quiescent-vc-git-grep)
(global-set-key (kbd "C-c C-r") #'quiescent-vc-git-grep)

;; 

;; ** iMenu

(global-set-key (kbd "M-i") #'imenu)

;; 

;; ** Project

(global-set-key (kbd "C-S-P") #'project-find-file)
(global-set-key (kbd "C-c p f") #'project-find-file)
(setq project-find-functions (list #'project-try-vc))

;; 

;; ** Goto Change

(use-package goto-chg
  :straight t
  :config (progn
            (bind-chord "'," 'goto-last-change)
            (bind-chord ",." 'goto-last-change-reverse)))

;; 

;; ** Lock Files & Backups

(make-variable-buffer-local 'create-lockfiles)

(setq backup-directory-alist `(("." . "~/.saves")))

(setq backup-by-copying t)

(setq delete-old-versions t
      kept-new-versions 8
      kept-old-versions 4
      version-control t)

(setq vc-make-backup-files t)

;; 

;; ** Smartscan

(use-package smartscan
  :straight t
  :demand t
  :hook ((org-mode . quiescent-smartscan-enable))
  :config (progn
            (defun quiescent-smartscan-enable ()
              "Enable smartscan."
              (when (and (null quiescent-starting-up)
                         (boundp 'smartscan-mode))
                (smartscan-mode 1)))
            (define-key smartscan-map (kbd "M-n") 'smartscan-symbol-go-forward)
            (define-key smartscan-map (kbd "M-p") 'smartscan-symbol-go-backward)
            (setq smartscan-symbol-selector "symbol")))

(defun quiescent-disable-smartscan ()
  "Disable smartscan mode."
  (when (and (null quiescent-starting-up)
             (boundp 'smartscan-mode))
    (smartscan-mode -1)))

(add-hook 'eshell-mode-hook #'quiescent-disable-smartscan)

;; 

;; ** Wgrep

(use-package wgrep
  :straight t)

(use-package wgrep-ag
  :straight t)

;; 

;; ** Go Up

(defun quiescent-one-directory-above-current ()
  "Produce the directory one above the current one."
  (file-name-directory (substring default-directory
                                  0
                                  (- (length default-directory)
                                     1))))

(defun quiescent-go-up ()
  "Do the thing which would most logically be considered going up in the current mode."
  (interactive)
  (cond
   ((eq major-mode 'eshell-mode)
    (progn
      (setq default-directory
            (quiescent-one-directory-above-current))
      (eshell-send-input)))
   ((eq major-mode 'dired-mode)
    (dired-up-directory))
   ((eq major-mode 'shell-mode)
    (progn
      (insert "cd ..")
      (comint-send-input)))
   ((eq major-mode 'minibuffer-inactive-mode)
    ;; TODO: Handle home dir
    (progn
      (end-of-line)
      (backward-char)
      (when (looking-at "/")
        (delete-char 1))
      (ignore-errors (forward-char))
      (zap-to-char -1 ?/)
      (insert "/")))
   (t
    (find-file default-directory))))

(global-set-key (kbd "C-^") #'quiescent-go-up)

;; 

;; ** Tick Number at Point

(defun quiescent-increment-number-at-point ()
  "Increment the number at point.

Assume that we're in a number.  Behaviour is undefined when we're
outside of one."
  (interactive)
  (save-excursion
    (progn
      (skip-chars-backward "0123456789")
      (let ((start-of-number (point))
            end-of-number
            number)
        (when (not (looking-at "[0-9]+"))
          (error "No number to increment"))
        (replace-match (format "%s" (1+ (string-to-number (match-string 0)))))))))

(defun quiescent-decrement-number-at-point ()
  "Increment the number at point.

Assume that we're in a number.  Behaviour is undefined when we're
outside of one."
  (interactive)
  (save-excursion
    (progn
      (skip-chars-backward "0123456789")
      (let ((start-of-number (point))
            end-of-number
            number)
        (when (not (looking-at "[0-9]+"))
          (error "No number to increment"))
        (replace-match (format "%s" (1- (string-to-number (match-string 0)))))))))

;; 

;; ** KMacro

(global-set-key (kbd "s-(") #'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "s-)") #'kmacro-end-or-call-macro)
(global-set-key (kbd "C-c r") 'replace-string)

;; ** Aligned Movement

(defmacro aligned-searcher (backward)
  "Create an aligned move function.

A non-nil BACKWARD argument means move backwards."
  `(let* ((indentation (buffer-substring (line-beginning-position)
                                         (save-excursion
                                           (move-beginning-of-line nil)
                                           (while (looking-at "[[:space:]]") (forward-char))
                                           (point))))
          (number-of-lines-to-move (- (save-excursion
                                        (,(if backward `search-backward-regexp `search-forward-regexp)
                                         (format "^%s[^[:space:]]" indentation) nil nil)
                                        (line-number-at-pos))
                                      (line-number-at-pos)))
          line-move-visual)
     (push-mark nil t)
     (line-move number-of-lines-to-move)))

(defun aligned-next ()
  "Move forward to the next line which is at the same indentation."
  (interactive)
  (aligned-searcher nil))

(defun aligned-previous ()
  "Move back to the next line which is at the same indentation."
  (interactive)
  (aligned-searcher t))

(defvar aligned-movement-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "s-n") #'aligned-next)
    (define-key keymap (kbd "s-p") #'aligned-previous)
    keymap)
  "The keymap for `aligned-search-mode'")

(define-minor-mode aligned-movement-mode ()
  "Enable bindings locally for aligned record movement.

Aligned record movement means moving backwards and forwards by
lines which have the same leved of indentation.  This is very
useful when inspecting call graphs and other similarly formatted
buffers.  (might be useful in programming languages like
Python!)

Forward is S-n.
Backward is S-p."
  :init-value nil
  :ligther nil
  :global nil)

;; 

;; ** Separedit

(use-package separedit
  :straight t
  :config
  (progn
    (define-key prog-mode-map (kbd "C-c '") #'separedit)
    (setq separedit-default-mode 'org-mode)))

;; 

;; 

;; ** Thought Stack

(define-minor-mode thought-stack-mode
  "Keep track of small related tasks.

e.g. when refactoring code, you might change the signature of a
function and then discover that the function is used in a related
way somewhere else.  If you want to switch train of thought, then
it might be a good idea to note what you were doing, or you could
note the related task before continuing.

This mode makes that easier by maintaing a stack of things to
take care of when you're done with what you're doing."
  :init-value nil
  :lighter    nil
  :keymap     (make-sparse-keymap)
  :global     t
  :group      'thought-stack)

(defvar thought-stack '()
  "The stack of related thoughts.")

(require 'cl-lib)

(defun thought-stack-window-points ()
  "Produce an map of the windows to their current points."
  (cl-loop
   with positions = (make-hash-table :test #'equal)
   for window being the windows
   do (puthash window (window-point window) positions)
   finally (return positions)))

(defun thought-stack-restore-window-points (window-points)
  "Restore the points that were set in WINDOW-POINTS."
  (cl-loop
   for window being the hash-keys of window-points
   do (set-window-point window (gethash window window-points))))

(defun thought-stack-grep-buffer-contents ()
  "Get the contents of the buffer called *grep* if it exists.

If the buffer doesn't exist, produce nil."
  (let ((grep-buffer (get-buffer "*grep*")))
    (when grep-buffer
      (save-window-excursion
        (switch-to-buffer grep-buffer)
        (buffer-substring (point-min) (point-max))))))

(defun thought-stack-push (concept)
  "Push CONCEPT onto the thought stack.

Saves the window configuration that's active now."
  (interactive "sPush: ")
  (push (list concept
              (current-window-configuration)
              (thought-stack-window-points)
              (thought-stack-grep-buffer-contents))
        thought-stack))

(defun thought-stack-restore-grep-buffer (grep-contents)
  "If GREP-CONTENTS is non nil, then set grep buffer to it."
  (when grep-contents
    (save-window-excursion
      (switch-to-buffer
       (get-buffer-create "*grep*"))
      (read-only-mode -1)
      (delete-region (point-min) (point-max))
      (insert grep-contents)
      (read-only-mode 1)
      (grep-mode))))

(defun thought-stack-pop ()
  "Pop the current concept from the thought stack.

Returns the window configuration to what it was when you pushed
the thought."
  (interactive)
  (let ((thought (pop thought-stack)))
    (if thought
        (progn
          (set-window-configuration (cadr thought))
          (thought-stack-restore-window-points (caddr thought))
          (thought-stack-restore-grep-buffer (cadddr thought))
          (read-key (format "You were doing: %s\nPress any key to continue..." (car thought))))
      (error "No thoughts left"))))

(defun thought-stack-view ()
  "Print the thought stack in a temporary buffer."
  (interactive)
  (progn
    (switch-to-buffer-other-window (get-buffer-create "*thought-stack*"))
    (special-mode)
    (read-only-mode -1)
    (delete-region (point-min) (point-max))
    (mapc (lambda (item) (insert (format " * %s\n" (car item)))) thought-stack)
    (read-only-mode 1)))

(define-key thought-stack-mode-map (kbd "s-[") #'thought-stack-push)
(define-key thought-stack-mode-map (kbd "s-]") #'thought-stack-pop)
(define-key thought-stack-mode-map (kbd "s-?") #'thought-stack-view)

(thought-stack-mode)

;; 

;; ** Dump Jump

(use-package dumb-jump
  :straight t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  :config
  (add-to-list 'dumb-jump-language-file-exts '(:language "javascript" :ext "svelte" "js")))

;; 

;; ** Capitalise Word

(require 'subr-x)
(require 'cl-lib)

(defun capitalise (word)
  "Produce a properly capitalised WORD."
  (format "%s%s"
          (upcase (substring word 0 1))
          (substring word 1)))

(defun quiescent-capital-snake-case-to-camel-case ()
  "Convert the variable name at POINT into camel case from capital snake case."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (thing  (thing-at-point 'symbol))
           (words  (thread-last
                     (split-string thing "_")
                     (mapcar (lambda (word) (downcase word))))))
      (goto-char (car bounds))
      (delete-region (car bounds) (cdr bounds))
      (insert (apply #'cl-concatenate
                     'string
                     (car words)
                     (mapcar #'capitalise (cdr words)))))))

;; 

;; ** iBuffer

(require 'ibuffer)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(setq ibuffer-formats
      '((mark modified read-only " "
              (name 35 35 :left :elide) ; change: 30s were originally 18s
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " " filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))

;; ** IsearchB

(define-key global-map [(control ?z)] 'isearchb-activate)

;; 

;; ** Hilight Block

(define-minor-mode highlight-block-mode
  "Highlight the containing block.

Should also highlight containing blocks when the buffer isn't focused."
  :init-value nil
  :lighter    nil
  :global     t
  :group      'highlight-block
  (if highlight-block-mode
      (setq highlight-block-idle-highlighter
            (run-with-idle-timer 0.5 t #'highlight-block-highlight-all-windows))
    (progn
      (mapc #'delete-overlay highlight-block-overlays)
      (setq highlight-block-overlays nil)
      (cancel-timer highlight-block-idle-highlighter)
      (setq highlight-block-idle-highlighter))))

(defvar highlight-block-idle-highlighter nil
  "The timer used to keep highlighting the current block(s).")

(defvar highlight-block-overlays nil
  "A list of overlays in all windows.")

(defun highlight-block-face ()
  "Produce the colour to highlight a block in."
  "#EEEEEE"
  ;; Can't get this to produce a suitable colour.
  ;; (apply #'color-rgb-to-hex
  ;;        (nconc (apply #'color-hsl-to-rgb
  ;;                      (apply #'color-darken-hsl
  ;;                             (nconc (apply #'color-rgb-to-hsl
  ;;                                           (mapcar (lambda (x) (/ x 65535.0))
  ;;                                                   (color-values (face-attribute 'default
  ;;                                                                                 :background))))
  ;;                                    (list 25))))
  ;;               (list 2)))
  )

(defun highlight-block-highlight-current-block ()
  "Highlight the block that the point is in."
  (save-excursion
    (ignore-errors (backward-up-list))
    (let* ((start (point))
           (end (progn (forward-sexp) (point)))
           (overlay (make-overlay start end)))
      (overlay-put overlay 'face `(:background ,(highlight-block-face)))
      (push overlay highlight-block-overlays))))

(defun highlight-block-highlight-all-windows ()
  "Highlight all blocks in all visible windows."
  (progn
   (mapc #'delete-overlay highlight-block-overlays)
   (setq highlight-block-overlays nil)
   (save-window-excursion
    (cl-loop
     for window being the windows
     do (select-window window)
     do (highlight-block-highlight-current-block)))))

;; 

;; ** Query Replace Multiple Matches
;;
;; Source: https://tony-zorman.com/posts/query-replace/2022-08-06-query-replace-many.html

(defun slot/get-queries (&optional pairs)
  "Get multiple `query-replace' pairs from the user.
PAIRS is a list of replacement pairs of the form (FROM . TO)."
  (-let* (((from to delim arg)
           (query-replace-read-args
            (s-join " "
                    (-non-nil
                     (list "Query replace many"
                           (cond ((eq current-prefix-arg '-) "backward")
                                 (current-prefix-arg         "word"))
                           (when (use-region-p) "in region"))))
            nil))                       ; no regexp-flag
          (from-to (cons (regexp-quote from)
                         (s-replace "\\" "\\\\" to))))
    ;; HACK: Since the default suggestion of replace.el will be
    ;; the last one we've entered, an empty string will give us
    ;; exactly that.  Instead of trying to fight against this,
    ;; use it in order to signal an exit.
    (if (-contains? pairs from-to)
        (list pairs delim arg)
      (slot/get-queries (push from-to pairs)))))

(defun slot/query-replace-many
    (pairs &optional delimited start end backward region-noncontiguous-p)
  "Like `query-replace', but query for several replacements.
Query for replacement pairs until the users enters an empty
string (but see `slot/get-queries').

Refer to `query-replace' and `perform-replace' for what the other
arguments actually mean."
  (interactive
   (let ((common (slot/get-queries)))
     (list (nth 0 common) (nth 1 common)
           (if (use-region-p) (region-beginning))
           (if (use-region-p) (region-end))
           (nth 2 common)
           (if (use-region-p) (region-noncontiguous-p)))))
  (perform-replace
   (concat "\\(?:" (mapconcat #'car pairs "\\|") "\\)") ; build query
   (cons (lambda (pairs _count)
           (cl-loop for (from . to) in pairs
                    when (string-match from (match-string 0))
                    return to))
         pairs)
   :query :regexp
   delimited nil nil start end backward region-noncontiguous-p))

;; 

;; * Languages

;; ** Json Mode

(use-package json-mode
  :straight t)

(require 'subr-x)

(defun quiescent-sort-json-array-by (start end field)
  "Sort the JSON array contained in the region [START, END] by a FIELD name."
  (interactive "r\nsField to sort by: ")
  (replace-region-contents
   start
   end
   (lambda () (let ((json-to-sort (buffer-substring-no-properties start end)))
                (with-temp-buffer
                  (shell-command
                   (cl-concatenate 'string
                                   "node --eval \"let x = JSON.parse('"
                                   (thread-last (string-replace "\""
                                                                "\\\""
                                                                json-to-sort)
                                                (string-replace "\'"
                                                                "\\\'")
                                                (string-replace "\n"
                                                                ""))
                                   "'); x.sort(function (a, b) { let av = a['"
                                   field
                                   "']; let bv = b['"
                                   field
                                   "']; if (av < bv) return -1; if (av > bv) return 1; return 0; }); console.log(JSON.stringify(x, null, 2));\"")
                   (current-buffer))
                  (buffer-substring-no-properties (point-min) (point-max)))))))

;; 

;; ** Julia Mode

(use-package julia-mode
  :straight t)

;; 

;; ** YAML Mode

(use-package yaml-mode
  :straight t)

;; 

;; ** Groovy Mode

(use-package groovy-mode
  :straight t)

;; 

;; ** C/C++ Mode

(require 'cc-mode)

(setq c-default-style "linux"
      c-basic-offset  4)
(setq auto-mode-alist (cons '("\.ino$" . c-mode) auto-mode-alist))

(defun quiescent-setup-c++-completion ()
  "Setup completion for c++ mode."
  (when (null quiescent-starting-up)
    (setq-local company-backends '(company-capf company-dabbrev))
    (setq-local completion-at-point-functions '(tags-completion-at-point-function))))

(add-hook 'c++-mode-hook #'quiescent-setup-c++-completion)

(defun quiescent-setup-c-completion ()
  "Setup completion for `c-mode'."
  (when (null quiescent-starting-up)
    (setq-local company-backends '(company-capf company-dabbrev))
    (setq-local completion-at-point-functions '(tags-completion-at-point-function))))

(add-hook 'c-mode-hook #'quiescent-setup-c-completion)

;; 

;; ** PlantUML Mode

(use-package plantuml-mode
  :straight t)

;; 

;; ** F Sharp Mode

(use-package fsharp-mode
  :straight t)

;; 

;; ** Graphviz Dot Mode

(use-package graphviz-dot-mode
  :straight t)

;; 

;; ** Web/HTML Mode(s)

(require 'sgml-mode)

(setq sgml-quick-keys 'close)

(use-package company-web
  :straight t
  :config (add-hook 'html-mode-hook #'quiescent-setup-company-web-completion))

(defun quiescent-setup-company-web-completion ()
  "Set the company backends for web completion."
  (when (null quiescent-starting-up)
    (setq-local company-backends '(company-web-html company-dabbrev))))

(use-package web-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))
  (with-eval-after-load "js2-mode"
    (progn
      (require 'js2-mode)
      (define-key web-mode-map (kbd "C-c C-'") #'quiescent-indirectly-edit-script-dwim)
      (define-key js2-mode-map (kbd "C-c C-'") #'quiescent-indirectly-edit-script-dwim))))

(with-eval-after-load 'flycheck
  (flycheck-add-mode 'html-tidy 'web-mode))

;; 

;; ** SASS Mode

(use-package sass-mode
  :straight t)

;; 

;; ** RJSX Mode

(use-package rjsx-mode
  :straight t
  :init (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode)))

(defun quiescent-electric-layout-create-tag (n &optional killflag)
  "When N is 1 and KILLFLAG nil, if next char is ?< then insert a newline."
  (when (and (eq n 1)
             (null killflag)
             (looking-at-p "<"))
    (progn
      (save-excursion
        (newline 2)
        (indent-for-tab-command))
      (forward-line 1)
      (indent-for-tab-command))))

(advice-add #'rjsx-delete-creates-full-tag :after #'quiescent-electric-layout-create-tag)

;; 

;; ** Powershell

(use-package powershell
  :straight t
  :config (add-to-list 'auto-mode-alist '("\\.ps1$" . powershell-mode)))

;; 

;; ** Haskell Mode

(defun quiescent-turn-on-haskell-doc-mode ()
  "Activate `haskell-doc-mode'."
  (when (null quiescent-starting-up)
    (turn-on-haskell-doc-mode)))

(use-package haskell-mode
  :straight t
  :config (add-hook 'haskell-mode-hook #'quiescent-turn-on-haskell-doc-mode))

(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t))

(custom-set-variables '(haskell-tags-on-save t))

(custom-set-variables
 '(haskell-process-type 'stack-ghci))

(with-eval-after-load "interactive-haskell-mode"
  (define-key interactive-haskell-mode-map (kbd "M-n") #'haskell-goto-next-error)
  (define-key interactive-haskell-mode-map (kbd "M-p") #'haskell-goto-prev-error))

(defun quiescent-disable-flycheck-mode ()
  "Disable flycheck mode."
  (when (null quiescent-starting-up)
    (flycheck-mode -1)))

(add-hook 'haskell-debug-mode-hook #'quiescent-disable-flycheck-mode)
(add-hook 'haskell-debug-mode-hook #'quiescent-disable-flyspell-mode)

;; 

;; ** Cython Mode

(use-package cython-mode
  :load-path "~/.emacs.d/cython")

;; 

;; ** NXML Mode

(require 'nxml-mode)

(defun pretty-print-xml (begin end)
  "Pretty format XML markup in [`BEGIN', `END'] region.

You need to have `nxml-mode'
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))

(defun quiescent-disable-flyspell-mode ()
  "Disable flyspell mode."
  (when (null quiescent-starting-up)
    (flyspell-mode -1)))

(defun quiescent-indent-four-if-pom-or-context ()
  "Setup indentation to use four spaces in nxml if I'm looking at a pom or context file."
  (when (null quiescent-starting-up)
    (let ((name (buffer-name)))
      (when (or (string-match-p "context.*xml" name)
                (string-match-p "pom.xml"      name))
        (setq nxml-child-indent 4
              nxml-attribute-indent 4)))))

(add-hook 'nxml-mode-hook #'quiescent-disable-flyspell-mode)
(add-hook 'nxml-mode-hook #'quiescent-disable-flycheck-mode)
(add-hook 'nxml-mode-hook #'quiescent-indent-four-if-pom-or-context)

(push "~/.emacs.d/schemas.xml" rng-schema-locating-files)

(setq nxml-slash-auto-complete-flag t)

;; 

;; ** Maven

(defun quiescent-nearest-pom-up ()
  "Goto the nearest pom file updwards from the current directory."
  (interactive)
  (let* ((start-directory (if (equal (file-relative-name (buffer-file-name)
                                                         default-directory) "pom.xml")
                              (quiescent-up-directory default-directory)
                            default-directory))
         (pom-dir (locate-dominating-file start-directory "pom.xml")))
    (when (not pom-dir)
      (error "Pom not found"))
    (find-file (concat pom-dir "pom.xml"))))

(require 'subr-x)

(defun quiescent-up-directory (dir)
  "Produce the directory one up from DIR.

Nil if root is supplied as DIR."
  (string-join (reverse (cddr (reverse (split-string dir "/")))) "/"))

(defun quiescent-maven-compile-on-nearest-pom-up (mvn-command)
  "Find the nearest pom up from the current directory and run MVN-COMMAND."
  (interactive "sMaven command to run: ")
  (let ((starting-buffer (buffer-name)))
    (progn
      (quiescent-nearest-pom-up)
      (compile (format "mvn -f %s %s" (buffer-file-name) mvn-command))
      (switch-to-buffer starting-buffer))))

;; 

;; ** JS2 Mode

(defun quiescent-indent-js-function ()
  "Indent the javascript function at point."
  (interactive)
  (if (null (flycheck-overlays-at (point)))
      (if (nth 4 (syntax-ppss))
          (fill-paragraph)
        (save-excursion
          (mark-defun)
          (call-interactively #'indent-for-tab-command)))
    (call-interactively #'quiescent-correct-linting-errors-at-point)))

(use-package js2-mode
  :straight t
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
            (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
            (define-key js2-mode-map (kbd "<f5>") #'quiescent-start-node-with-this-file)
            (define-key js2-mode-map (kbd "<f6>") #'quiescent-start-mocha-with-this-file)
            (define-key js2-mode-map (kbd "<f7>") #'quiescent-start-jest-with-this-file)
            (define-key js2-mode-map (kbd "M-q") #'quiescent-indent-js-function)))

(use-package js2-refactor
  :straight t)

(defun quiescent-pluse-ignoring-args (&rest _)
  "Run `xref-pulse-momentarily' ignoring ARGS."
  (xref-pulse-momentarily))

(advice-add #'js2-jump-to-definition :after #'quiescent-pluse-ignoring-args)

(advice-add #'js2-jump-to-definition :before #'quiescent-xref-push-marker-stack)

(defun quiescent-xref-jump-when-point-doesnt-move (jump-fun &rest args)
  "Execute JUMP-FUN with ARGS, jump with xref if it didn't move."
  (let ((start (point)))
    (ignore-errors (apply jump-fun args))
    (when (eql start (point))
      (xref-find-definitions (thing-at-point 'symbol)))))

;; I use global nowadays
;; (advice-add #'js2-jump-to-definition :around #'quiescent-xref-jump-when-point-doesnt-move)

(defvar quiescent-javascript-eslint-fix-modes '(js2-mode web-mode js-jsx-mode rjsx-mode)
  "The modes which should have eslint --fix run on their files.")

(defun quiescent-javascript-eslint-this-file ()
  "Eslint this file with fix turned on."
  (interactive)
  (when (member major-mode quiescent-javascript-eslint-fix-modes)
    (setq quiescent-buffer-to-revert (current-buffer))
    (async-start-process (format "eslint-%s" (buffer-file-name))
                         "eslint"
                         (lambda (process) (auto-revert-buffers))
                         "--fix"
                         (buffer-file-name))))

(defun quiescent-javascript-prettier-fix-this-file ()
  "Fix all prettier errors on this file."
  (interactive)
  (progn
    (async-start-process (format "prettier:%s" (buffer-file-name))
                         "prettier"
                         (lambda (process) (auto-revert-buffers))
                         (format "--plugin-search-dir=%s" (file-truename (project-root (project-current))))
                         "--write"
                         (buffer-file-name))))

;; TODO account for multiple checkers reporting on the current line
(defun quiescent-correct-linting-errors-at-point (point)
  "Correct linting errors reported by Eslint via Flycheck at POINT."
  (interactive "d")
  (let* ((error-overlay (car (flycheck-overlays-at (point))))
         (lint-suggestion (when error-overlay
                            (flycheck-error-message
                             (overlay-get error-overlay
                                          'flycheck-error)))))
    (if lint-suggestion
        (pcase (split-string lint-suggestion)
          (`("Delete" ,to-delete) (quiescent-correct-linting-delete to-delete
                                                                    error-overlay))
          (`("Replace" ,string "with" ,replacement)
           (quiescent-correct-linting-replace string replacement error-overlay))
          (`("Insert" ,string)
           (quiescent-correct-linting-insert string error-overlay)))
      (fill-paragraph))))

(defun quiescent-correct-linting-insert (string error-overlay)
  "Insert STRING at the start of ERROR-OVERLAY."
  (save-excursion
    (goto-char (overlay-start error-overlay))
    (insert (quiescent-eslint-string-to-regexp
             (quiescent-get-string-out-of-eslint-quotes string)))))

(defun quiescent-correct-linting-compute-search-string (raw-search-string)
  "Escape RAW-SEARCH-STRING from Eslint so that it can be searched for."
  (regexp-quote
   (quiescent-get-string-out-of-eslint-quotes
    (quiescent-eslint-string-to-regexp raw-search-string))))

(defun quiescent-correct-linting-replace (string replacement error-overlay)
  "Replace STRING with REPLACEMENT at the start of ERROR-OVERLAY."
  (let ((regexp-to-delete (quiescent-correct-linting-compute-search-string string)))
    (save-excursion
      (goto-char (overlay-start error-overlay))
      (when (re-search-forward regexp-to-delete
                               nil
                               t)
        (replace-match (quiescent-get-string-out-of-eslint-quotes
                        (quiescent-eslint-string-to-regexp replacement)))))))

(defun quiescent-get-string-out-of-eslint-quotes (eslint-string-in-quotes)
  "Get the ESLINT-STRING-IN-QUOTES from out of it's `` quotes."
  (substring eslint-string-in-quotes 1 (1- (length eslint-string-in-quotes))))

(defun quiescent-eslint-string-to-regexp (eslint-string)
  "Transform ESLINT-STRING to a regexp."
  (replace-regexp-in-string "⏎" "\n"
                            (replace-regexp-in-string "·" " "
                                                      eslint-string)))

(defun quiescent-correct-linting-delete (to-delete error-overlay)
  "Find and delete the string TO-DELETE in the bounds of ERROR-OVERLAY."
  (let ((regexp-to-delete (quiescent-correct-linting-compute-search-string to-delete)))
    (save-excursion
      (goto-char (overlay-start error-overlay))
      (when (re-search-forward regexp-to-delete
                               nil
                               t)
        (replace-match "")))))

(use-package js2-highlight-vars
  :straight t)

(defun quiescent-activate-js2-highlight-vars-mode ()
  "Activate `js2-highlight-vars'."
  (when (null quiescent-starting-up)
    (js2-highlight-vars-mode 1)))

(defun quiescent-catch-no-js-ast-error (f &rest args)
  "Run F and catch any errors."
  (ignore-error (error "No JavaScript AST available")
    (apply f args)))

(advice-add 'js2--do-highlight-vars :around #'quiescent-catch-no-js-ast-error)

(add-hook #'js2-mode-hook #'quiescent-activate-js2-highlight-vars-mode)

(defun js2-highlight-vars-post-command-hook ()
  (ignore-errors
    (let* ((overlays (overlays-at (point)))
           (ovl (and overlays
                     (catch 'found
                       (dolist (ovl overlays)
                         (when (overlay-get ovl 'js2-highlight-vars)
                           (throw 'found ovl)))
                       nil))))
      (if (and ovl
               (string= js2--highlight-vars-current-token-name
                        (buffer-substring (overlay-start ovl)
                                          (overlay-end ovl))))
          (setq js2--highlight-vars-current-token (overlay-start ovl))
        (js2--unhighlight-vars)
        (when js2--highlight-vars-post-command-timer
          (cancel-timer js2--highlight-vars-post-command-timer))
        (setq js2--highlight-vars-post-command-timer
              (run-with-timer 0 nil 'js2--do-highlight-vars))))))

(defun quiescent-activate-js2-imenu-extras ()
  "Activate `js2-imenu-extras-mode'."
  (when (null quiescent-starting-up)
    (js2-imenu-extras-mode 1)))

(add-hook 'js2-mode-hook #'quiescent-activate-js2-imenu-extras)

(defun quiescent-js2-raise-variable (p)
  "Raise the variable at point, P."
  (interactive "d")
  (let* ((node          (ignore-errors (js2-node-at-point)))
         (node-start    (and node (js2-node-abs-pos node)))
         (up-list-point (save-excursion (ignore-errors (backward-up-list)) (point)))
         (expression    (if (or (null node) (= up-list-point node-start))
                            (let ((simple-node (js2-node-at-point)))
                              (buffer-substring (js2-node-abs-pos simple-node)
                                                (js2-node-abs-end simple-node)))
                          (buffer-substring node-start (js2-node-abs-end node)))))
    (if (eq 'rjsx-node (type-of (js2-node-at-point)))
        (goto-char (js2-node-abs-pos (js2-node-parent (js2-node-at-point))))
      (backward-up-list))
    (let ((node-to-replace (js2-node-at-point)))
      (if (eq (type-of node-to-replace) 'js2-import-clause-node)
          (kill-sexp)
        (delete-region (js2-node-abs-pos node-to-replace) (js2-node-abs-end node-to-replace))))
    (save-excursion (insert expression))))

(define-key js2-highlight-vars-local-keymap (kbd "M-r") nil)

(define-key js2-mode-map (kbd "M-r") #'quiescent-js2-raise-variable)

(defun quiescent-js-dependency-graph-for-directory ()
  "Compute the dependency graph between JS modules in DEFAULT-DIRECTORY."
  (interactive)
  (let ((dependencies (make-hash-table))
        (only-this-directory (y-or-n-p "Only this directory?")))
    (cl-labels ((to-dot (dependencies)
                  (switch-to-buffer-other-window
                   (get-buffer-create "*dependencies*"))
                  (delete-region (point-min) (point-max))
                  (insert "digraph dependencies {\n")
                  (dolist (key (hash-table-keys dependencies))
                    (dolist (value (gethash key dependencies))
                      (insert (format "  %s -> %s;\n" key value))))
                  (insert "}"))
                (last-slash-pos (dependency)
                  (cl-position ?\/ dependency :from-end t))
                (strip-path (dependency)
                  (let ((last-slash-pos (last-slash-pos dependency)))
                    (substring dependency (if last-slash-pos
                                              (1+ last-slash-pos)
                                            0))))
                (current-file-dependencies ()
                  (when (re-search-forward "}.* from '\\(.*\\)'" nil t)
                    (let ((next-match (thread-last (match-string 1)
                                                   strip-path
                                                   read-from-string
                                                   car)))
                      (if (or (and only-this-directory
                                   (eq 1 (last-slash-pos (match-string 1))))
                              (not only-this-directory))
                          (cons next-match
                                (current-file-dependencies))
                        (current-file-dependencies)))))
                (recur (files)
                  (if (null files)
                      (to-dot dependencies)
                    (let ((file (car files)))
                      (with-temp-buffer
                        (insert-file file)
                        (setf (gethash (thread-last (file-name-base file)
                                                    read-from-string
                                                    car)
                                       dependencies)
                              (current-file-dependencies)))
                      (recur (cdr files))))))
      (thread-last (directory-files default-directory)
                   (cl-remove-if #'file-directory-p)
                   recur))))

(defun quiescent-js-selector-dependency-graph ()
  "Produce a graph of the dependencies between selectors in the current buffer."
  (interactive)
  (cl-labels ((to-dot (dependencies)
                (switch-to-buffer-other-window
                 (get-buffer-create "*dependencies*"))
                (delete-region (point-min) (point-max))
                (insert "digraph dependencies {\n")
                (dolist (entry dependencies)
                  (dolist (value (cdr entry))
                    (insert (format "  %s -> %s;\n" (car entry) value))))
                (insert "}"))
              (selector-dependencies ()
                (progn
                  (forward-sexp)
                  (when (looking-at-p ",")
                    (cons (thing-at-point 'symbol)
                          (selector-dependencies)))))
              (recur ()
                (let ((hit (when (re-search-forward "const [a-zA-Z]+ = createSelector(" nil t)
                             (progn
                               (search-backward "const")
                               (forward-word 2)
                               (prog1 (thing-at-point 'symbol)
                                 (search-forward "createSelector("))))))
                  (when hit
                    (cons (cons hit (selector-dependencies))
                          (recur))))))
    (save-excursion
      (goto-char (point-min))
      (thread-last (recur)
                   to-dot))))

(defun quiescent-parse-digraph ()
  "Parse the dot digraph in the current buffer."
  (let ((graph (make-hash-table :test #'equal)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "-> " nil t)
        (push (thing-at-point 'symbol)
              (gethash (save-excursion (back-to-indentation)
                                       (thing-at-point 'symbol))
                       graph))))
    graph))

(defun quiescent-dot-descendants-of (root)
  "Produce the subgraph of the dot graph in the current buffer rooted at ROOT."
  (interactive "sRoot: ")
  (let ((graph (quiescent-parse-digraph)))
    (cl-labels ((to-dot (node)
                  (dolist (destination (gethash node graph))
                    (insert (format "  %s -> %s;\n" node destination))
                    (to-dot destination))))
      (switch-to-buffer-other-window
       (get-buffer-create "*dependencies*"))
      (delete-region (point-min) (point-max))
      (insert (format "digraph %s {\n" root))
      (to-dot root)
      (insert "}")
      (goto-char (point-min))
      (delete-duplicate-lines (point-min) (point-max)))))

;; 

;; ** Tide Mode

;; (use-package tide
;;   :straight t
;;   :config
;;   (progn
;;     (defun quiescent-reboot-tide-on-error (f response)
;;       "Advice around F (`tide-dispatch-response') to detect missing RESPONSE.

;; Restarts the tide server when it finds it."
;;       (let ((message (plist-get response :message)))
;;         (if (and message (string-match "Could not find source file" message))
;;             (tide-restart-server)
;;           (funcall f response))))
;;     (advice-add 'tide-dispatch-response :around #'quiescent-reboot-tide-on-error))
;;   :init
;;   (progn
;;     (add-hook 'js2-mode-hook        #'quiescent-setup-tide-mode)
;;     (add-hook 'js2-jsx-mode-hook    #'quiescent-setup-tide-mode)
;;     (add-hook 'typescript-mode-hook #'quiescent-setup-tide-mode)))

;; (defun quiescent-tide-jump-to-definition (&optional arg)
;;   "Jump to the definition of the symbol at point.

;; If pointed at an abstract member-declaration, will proceed to look for
;; implementations.  When invoked with a prefix arg, jump to the type definition.

;; Copied from tide's sources with the addition of calling
;; js2-mode's find definition and then xref when tide fails."
;;   (interactive "P")
;;   (let ((cb (lambda (response)
;;               (if (and (tide-response-success-p response)
;;                        (null (plist-get response :message)))
;;                   (condition-case err
;;                       (js2-jump-to-definition)
;;                     (error (xref-find-definitions (thing-at-point 'symbol))))
;;                 (tide-on-response-success response
;;                     (-when-let (filespan (car (plist-get response :body)))
;;                       ;; if we're still at the same location...
;;                       ;; maybe we're a abstract member which has implementations?
;;                       (if (and (not arg)
;;                                (tide-filespan-is-current-location-p filespan))
;;                           (tide-jump-to-implementation)
;;                         (tide-jump-to-filespan filespan tide-jump-to-definition-reuse-window))))))))
;;     (if arg
;;         (tide-command:typeDefinition cb)
;;       (tide-command:definition cb))))

;; (defun quiescent-setup-tide-mode ()
;;   "Setup TIDE mode."
;;   (when (null quiescent-starting-up)
;;     (progn
;;       (tide-setup)
;;       (define-key tide-mode-map (kbd "M-?") 'tide-references)
;;       (tide-hl-identifier-mode +1)
;;       (setq-local company-backends '(company-tide company-files))
;;       (advice-add #'tide-jump-to-definition :before  #'xref-push-marker-stack)
;;       (define-key tide-mode-map (kbd "M-.") #'quiescent-tide-jump-to-definition)
;;       (setq xref-backend-functions '(dumb-jump-xref-activate etags--xref-backend)))))

;; 

;; Tern Mode

(add-to-list 'load-path "~/frm-src/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)

(defun quiescent-enable-tern-mode ()
  "Enabled tern mode."
  (tern-mode t))

(with-eval-after-load 'tern
  (define-key tern-mode-keymap (kbd "M-.") #'js2-jump-to-definition)
  (define-key tern-mode-keymap (kbd "M-,") #'xref-go-back))

(add-hook 'js-mode-hook #'quiescent-enable-tern-mode)

;; 

;; ** Typescript Mode

(use-package typescript-mode
  :straight t
  :config (add-to-list 'auto-mode-alist '("\\.tsx$" . typescript-mode)))

;; 

;; ** Rust Mode

(use-package rust-mode
  :straight t)

(use-package rustic
  :straight t
  :config
  (progn
    (setq rustic-lsp-server 'rust-analyzer)
    (setq lsp-rust-analyzer-server-command '("~/.cargo/bin/rust-analyzer"))
    (setq rustic-lsp-client 'eglot)
    (remove-hook 'rustic-mode-hook #'flycheck-mode)
    (remove-hook 'rustic-mode-hook #'flymake-mode-off)
    (add-hook 'rustic-mode-hook #'quiescent-enable-flymake-mode)
    (add-hook 'rustic-mode-hook #'quiescent-disable-flycheck)
    (define-key rust-mode-map (kbd "M-n") #'flymake-goto-next-error)
    (define-key rust-mode-map (kbd "M-p") #'flymake-goto-prev-error)))

(defun quiescent-enable-rustic-mode ()
  "Enable rustic mode."
  (rustic-mode))

(add-hook 'rust-ts-mode-hook #'quiescent-enable-rustic-mode)

(defun quiescent-indent-defun-or-fill-paragraph ()
  "Indent the defun that point is in.

If the point is inside a comment then fill paragraph on the
comment."
  (interactive)
  (save-excursion
    (if (nth 4 (syntax-ppss))
        (fill-paragraph)
      (progn
        (mark-defun)
        (indent-region (region-beginning) (region-end))))))

(define-key rustic-mode-map (kbd "M-q") #'quiescent-indent-defun-or-fill-paragraph)

(defun rustic-mode--find-project-root (directory)
  "Produce the root of Rust project containing DIRECTORY."
  (let ((rustic-root (locate-dominating-file directory "cargo.toml")))
    (when rustic-root
      (cons 'transient rustic-root))))

(require 'project)

(add-hook 'project-find-functions #'rustic-mode--find-project-root)

;; 

;; ** Slime Mode

(use-package slime-company
  :straight t)

(defun quiescent-setup-slime-completion ()
  "Setup company for slime."
  (when (null quiescent-starting-up)
    (setq-local company-backends '(company-capf company-slime))
    (setq-local completion-at-point-functions '(quiescent-slime-completion-at-point))))

(defun quiescent-slime-completion-at-point ()
  "A completion at point function for slime.

Based on `slime-expand-abbreviations-and-complete' from
`slime-c-p-c'."
  (let* ((end (move-marker (make-marker) (slime-symbol-end-pos)))
         (beg (move-marker (make-marker) (slime-symbol-start-pos)))
         (prefix (buffer-substring-no-properties beg end)))
    (pcase (slime-contextual-completions beg end)
      (`(,completions . ,_) `(,beg ,end ,completions . (:exclusive 'no))))))

(require 'cl-lib)

(defun quiescent-close-slime-help ()
  "Close the help window."
  (interactive)
  (let ((original-window (selected-window)))
    (cl-loop
     for window being the windows
     when (or (equal "*slime-description*" (buffer-name (window-buffer window)))
              (equal "*slime-compilation*" (buffer-name (window-buffer window))))
     do (select-window window)
     (call-interactively #'quit-window))
    (select-window original-window)))

(defun quiescent-slime-company-doc-mode (&rest _)
  "Fontify the current buffer as a Slime documentation buffer."
  (run-at-time 0.1 nil
               (lambda () (let ((original-window (selected-window)))
                            (cl-loop
                             for window being the windows
                             when (equal "*slime-description*" (buffer-name (window-buffer window)))
                             do (select-window window)
                             (slime-company-doc-mode))
                            (select-window original-window)))))

;; These forms need to be protected from compilation so that they use
;; dynamic scoping for the repl history file.
(eval `(defun quiescent-slime-load-history (&optional path)
         "Read the Slime history file interactively or from PATH."
         (interactive (list (let ((slime-repl-history-file (concatenate 'string
                                                                        default-directory
                                                                        ".slime-history.eld")))
                              (slime-repl-read-history-filename))))
         (slime-repl-load-history path)))

(eval `(defun quiescent-slime-save-history (&optional path)
         "Read the Slime history file interactively or from PATH."
         (interactive (list (let ((slime-repl-history-file (concatenate 'string
                                                                        default-directory
                                                                        ".slime-history.eld")))
                              (slime-repl-read-history-filename))))
         (slime-repl-save-history path)))

(require 'xref)

(defun quiescent-xref-push-marker-stack (&rest _)
  "Push the current point onto the xref marker stack."
  (xref-push-marker-stack (point)))

(defun quiescent-slime-edit-definition (name)
  "Find the definition of NAME, falling back to xref when we fail."
  (interactive (list (or (and (not current-prefix-arg)
                              (slime-symbol-at-point))
                         (slime-read-symbol-name "Edit Definition of: "))))
  (condition-case nil
      (slime-edit-definition name)
    (error (xref-find-definitions name))))

(use-package slime
  :straight t
  :config
  (progn
    (define-key slime-mode-map (kbd "s-q") #'quiescent-close-slime-help)
    (define-key slime-mode-map (kbd "M-.") #'quiescent-slime-edit-definition)
    (advice-add 'slime-describe-symbol :after #'quiescent-slime-company-doc-mode)
    (advice-add 'slime-describe-function :after #'quiescent-slime-company-doc-mode)
    (advice-add 'slime-edit-definition :before #'quiescent-xref-push-marker-stack)

    (require 'slime-repl)
    (define-key slime-repl-mode-map (kbd "C-x C-s") #'quiescent-slime-save-history)
    (define-key slime-repl-mode-map (kbd "C-x C-f") #'quiescent-slime-load-history)
    (define-key slime-repl-mode-map (kbd "C-M-r")   #'slime-repl-previous-matching-input)
    (add-hook 'slime-mode-hook      #'quiescent-setup-slime-completion)
    (add-hook 'slime-repl-mode-hook #'quiescent-setup-slime-completion))
  :init (progn (setq inferior-lisp-program "sbcl")
               (setq auto-mode-alist (cons '("\.cl$" . common-lisp-mode) auto-mode-alist))))

(defun quiescent-setup-slime ()
  "Setup slime."
  (slime-setup '(slime-fancy slime-company slime-c-p-c slime-highlight-edits slime-xref-browser slime-asdf)))

(add-hook 'after-init-hook #'quiescent-setup-slime)

(defun quiescent-slime-restart-advice (f &rest args)
  "Wrap call to Slime restart function, F, with ARGS saving window state."
  (save-window-excursion
    (apply f args)))

(advice-add #'slime-restart-sentinel :around #'quiescent-slime-restart-advice)

(defun slime-qlot-exec (directory)
  "Exec Slime in DIRECTORY with qlot.

Source: https://github.com/fukamachi/qlot"
  (interactive (list (read-directory-name "Project directory: ")))
  (slime-start :program "qlot"
               :program-args '("exec" "ros" "-S" "." "run")
               :directory directory
               :name 'qlot
               :env (list (concat "PATH=" (mapconcat 'identity exec-path ":")))))

;; 

;; ** Clojure

(defun quiescent-setup-clojure-hooks ()
  "Setup hooks for clojure mode."
  (when (null quiescent-starting-up)
    (cider-mode 1)
    (eldoc-mode 1)
    ))

(use-package cider
  :straight t
  :config
  (progn
    (add-hook 'clojure-mode-hook    #'quiescent-setup-clojure-hooks)
    (add-hook 'cider-repl-mode-hook #'quiescent-setup-cider-repl-hooks)
    (define-key cider-mode-map (kbd "M-n") #'cider-jump-to-compilation-error)
    (define-key cider-mode-map (kbd "M-p") #'cider-jump-to-compilation-error)
    (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api)
                          (start-figwheel!)
                          (cljs-repl))")))

(use-package macrostep
  :straight t)

(defun quiescent-debug-on-all-signals ()
  "Trigger the debugger on all errors whether or not they are in an error handler."
  (interactive)
  (progn
    (setq debug-on-signal t)
    (debug-on-error)))

(use-package eros
  :straight t)

;; 

;; ** Emacs Lisp

(define-key emacs-lisp-mode-map (kbd "C-c C-z") #'quiescent-switch-to-ielm-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c C-k") #'eval-buffer)

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

(defun quiescent-eros-mode ()
  "Enable `eros-mode'."
  (when (null quiescent-starting-up)
    (eros-mode 1)))

(add-hook 'emacs-lisp-mode-hook #'quiescent-eros-mode)

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

;; 

;; ** Scala

(use-package scala-mode
  :straight t
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :straight t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(add-hook 'sbt-mode-hook (lambda ()
                           (add-hook 'before-save-hook 'sbt-hydra:check-modified-buffers)))

(add-hook 'scala-mode-hook #'quiescent-remove-flex)

(defun quiescent-ensure-eglot ()
  "Enable eglot in the curernt buffer."
  (when (null quiescent-starting-up)
    (eglot-ensure)))

(defun scala-mode--find-project-root (directory)
  "Produce the root of SBT project containing DIRECTORY.

Assumes that you use SBT."
  (when (null quiescent-starting-up)
    (let ((sbt-root (locate-dominating-file directory "build.sbt")))
      (when sbt-root
        (cons 'sbt sbt-root)))))

(add-hook 'project-find-functions #'scala-mode--find-project-root)

(cl-defmethod project-roots ((project (head sbt)))
  (list (cdr project)))

(cl-defmethod project-files ((project (head sbt)) &optional dirs)
  (project-files (project-try-vc (car (project-roots project))) dirs))

;; 

;; ** Dockerfile Mode

(use-package dockerfile-mode
  :straight t)

;; 

;; ** Kotlin Mode

(use-package kotlin-mode
  :straight t)

;; 

;; ** Terraform Mode

(use-package terraform-mode
  :straight t)

;; 

;; ** Python

(use-package pyvenv
  :load-path "~/.emacs.d/lisp/pyvenv/")

(defun quiescent-python-use-conda ()
  "Use an anaconda environment for python development."
  (interactive)
  (progn
    (setenv "WORKON_HOME" "~/anaconda3/envs")
    (pyvenv-mode 1)))

(defun quiescent-python-use-spark ()
  "Use spark to start python shells."
  (interactive)
  (setq python-shell-interpreter "pyspark"))

(defun quiescent-setup-python-company-completion ()
  "Setup completion for use with Python."
  (setq-local company-backends
              '(company-capf company-dabbrev-code company-files company-keywords))
  (setq-local completion-at-point-functions
              '(python-completion-at-point)))

(add-hook 'python-mode-hook #'quiescent-setup-python-company-completion)

;; 

;; ** Racket Mode

(use-package racket-mode
  :straight t)

;; 

;; ** Ruby Mode

(use-package enh-ruby-mode
  :straight t
  :config
  (progn
    (autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
    (add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
    (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))))

(defun quiescent-setup-ruby-completion ()
  "Setup completion for `enh-ruby-mode'."
  (when (null quiescent-starting-up)
    (setq-local company-backends '(company-robe company-capf company-dabbrev))
    (setq-local completion-at-point-functions '(tags-completion-at-point-function))))

(use-package robe
  :straight t
  :config
  (progn
    (add-hook 'enh-ruby-mode-hook #'robe-mode)
    (add-hook 'enh-ruby-mode-hook #'quiescent-setup-ruby-completion)))

;; 

;; ** ESS (R etc.)

(use-package ess
  :straight t)

;; 

;; ** Swift Mode

(use-package swift-mode
  :straight t)

;; 

;; ** CSV mode

(use-package csv-mode
  :straight t)

;; 

;; * Tree Sitter

;; This has its own section after Languages because it's likely to
;; depend on languages.

(use-package treesit-parser-manager
  :straight (treesit-parser-manager
             :host codeberg
             :repo "ckruse/treesit-parser-manager"
             :files ("*.el"))
  :commands (treesit-parser-manager-install-grammars
             treesit-parser-manager-update-grammars
             treesit-parser-manager-install-or-update-grammars
             treesit-parser-manager-remove-grammar)
  :custom
  (treesit-parser-manager-grammars
   '(("https://github.com/tree-sitter/tree-sitter-rust"
      ("tree-sitter-rust"))

     ("https://github.com/ikatyang/tree-sitter-toml"
      ("tree-sitter-toml"))

     ("https://github.com/tree-sitter/tree-sitter-typescript"
      ("tree-sitter-typescript/tsx" "tree-sitter-typescript/typescript"))

     ("https://github.com/tree-sitter/tree-sitter-javascript"
      ("tree-sitter-javascript"))

     ("https://github.com/tree-sitter/tree-sitter-css"
      ("tree-sitter-css"))

     ("https://github.com/serenadeai/tree-sitter-scss"
      ("tree-sitter-scss"))

     ("https://github.com/tree-sitter/tree-sitter-json"
      ("tree-sitter-json"))))
  :config
  (add-to-list 'treesit-extra-load-path treesit-parser-manager-target-directory)
  :hook (emacs-startup . treesit-parser-manager-install-grammars))


;; * Post Programming Languages Config

;; ** Flycheck

(require 'flycheck)

(flycheck-define-checker html-jshint
  "A JavaScript linter for html."
  :command ("jshint" "--extract=auto" "--reporter=checkstyle" source)
  :error-parser flycheck-parse-checkstyle
  :error-filter
  (lambda (errors)
    (flycheck-remove-error-file-names
     "stdin" (flycheck-dequalify-error-ids errors)))
  :modes (web-mode))

(flycheck-add-next-checker 'html-tidy 'html-jshint)

(add-to-list 'flycheck-checkers 'html-jshint t)

;; Courtesy: http://daniel-bush.blogspot.co.za/2014/12/emacs-flycheck-and-jshint-and-other.html
(flycheck-define-checker javascript-jscs
  "A JavaScript style checker using jscs.

See URL `https://www.npmjs.com/package/jscs'."
  :command ("jscs" "--reporter=checkstyle"
            (config-file "--config" flycheck-jscsrc)
            source)
  :error-parser flycheck-parse-checkstyle
  :modes (js-mode js2-mode js3-mode js-jsx-mode))

(flycheck-def-config-file-var flycheck-jscsrc javascript-jscs ".jscsrc"
  :safe #'stringp)

(add-to-list 'flycheck-checkers 'javascript-jscs t)
(flycheck-add-next-checker 'javascript-jshint '(t . javascript-jscs))
(flycheck-add-next-checker 'javascript-jshint '(t . javascript-eslint))

(flycheck-add-mode 'javascript-jshint #'js-jsx-mode)

(add-hook 'js2-mode-hook #'quiescent-select-jshint)
(add-hook 'rjsx-mode-hook #'quiescent-select-eslint)

(defun quiescent-select-eslint ()
  "Select EsLint as the linter in this buffer."
  (interactive)
  (when (null quiescent-starting-up)
    (flycheck-select-checker 'javascript-eslint)))

;; From https://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint
          (and root
               (expand-file-name (format "node_modules/.bin/eslint%s"
                                         (if (eq system-type 'windows-nt) ".cmd" ""))
                                 root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(defun quiescent-select-jshint ()
  "Select JSHint as the linter in this buffer."
  (interactive)
  (when (and (null quiescent-starting-up)
             (not (eq flycheck-checker 'javascript-eslint))
             (not (eq major-mode 'rjsx-mode)))
    (flycheck-select-checker 'javascript-jshint)))

;; 

;; ** Eglot

(use-package eglot
  :straight t
  :config
  (progn
    (add-to-list 'eglot-server-programs '(scala-mode . ("metals-emacs")))
    (setq eglot-server-programs
          (map-delete eglot-server-programs 'rustic-mode))
    (add-to-list 'eglot-server-programs '(rustic-mode . ("rust-analyzer")))
    (add-hook 'scala-mode-hook #'eglot-ensure)
    (add-hook 'rust-mode-hook  #'eglot-ensure)
    (define-key eglot-mode-map (kbd "M-RET") #'eglot-code-actions)))

;; ** LSP

(use-package lsp-mode
  :straight t
  :init (progn
          (defun quiescent-setup-svelte ()
            "Setup svelte if we're in a file with that exten."
            (progn
              (setq-local font-lock-maximum-decoration 1)
              (when (thread-last (buffer-file-name (current-buffer))
                                 file-name-extension
                                 (string-equal "svelte"))
                (lsp)
                (lsp-headerline-breadcrumb-mode -1))))
          (add-hook 'web-mode-hook #'quiescent-setup-svelte)))

;; 

;; ** Markdown Mode

(use-package markdown-mode
  :straight t)

;; 

;; ** GGTags

(use-package ggtags
  :straight t
  :hook ((js2-mode . ggtags-mode)
         (rjsx-mode . ggtags-mode)
         (java-mode . ggtags-mode))
  :config (progn
            (define-key ggtags-mode-map (kbd "M-.") #'quiescent-js2-jump-then-ggtags-find-other-symbol)))

(defun quiescent-js2-jump-then-ggtags-find-other-symbol (&optional arg)
  "Use xref to find the definition at point with failover.

Give ARG to ggtags."
  (interactive (list (ggtags-read-tag 'symbol current-prefix-arg)))
  (condition-case err
      (let ((old-point (point)))
        (funcall-interactively #'js2-jump-to-definition)
        (when (= (point) old-point)
          (ggtags-find-other-symbol arg)))
    (error (ggtags-find-other-symbol arg))))

;; * Applications

;; ** Shell

(defvar quiescent-supress-cd nil
  "Whether to not cd to the directory where shell was called.")

(defun quiescent-shell-here (call-shell &rest args)
  "Start shell with CALL-SHELL and ARGS.
Then switch directory to where we called it from"
  (let ((target-directory default-directory))
    (apply call-shell args)
    (unless quiescent-supress-cd
      (run-at-time 0.1 nil (lambda ()
                             (insert (format "cd %s" target-directory))
                             (comint-send-input))))))

(advice-add #'shell :around #'quiescent-shell-here)

(defun quiescent-unique-shell (name)
  "Create a new shell in this directory with a user specified NAME."
  (interactive "sShell name: ")
  (shell (format "*shell: %s*" name)))

(global-set-key (kbd "<f5>") #'quiescent-unique-shell)

;; ** Eshell

(require 'em-prompt)
(require 'vc-git)

(defconst quiescent-eshell-project-buffer-regexp "\*eshell.*\*"
  "A regular expression which matches project eshell buffer names.")

(require 'eshell)

(defun quiescent-eshell ()
  "Create split current window and make bottom half an eshell instance."
  (interactive)
  (if (or (string-equal (buffer-name) eshell-buffer-name)
          (string-match quiescent-eshell-project-buffer-regexp (buffer-name)))
      (delete-window)
    (quiescent-eshell-switch-to-and-change-dir)))

(defun quiescent-eshell-smart-initialise ()
  "Initialise `eshell-smart'."
  (when (null quiescent-starting-up)
    (eshell-smart-initialize)))

(use-package em-smart
  :chords ((",s" . quiescent-eshell))
  :config (progn
            (setq eshell-where-to-jump 'begin)
            (setq eshell-review-quick-commands nil)
            (setq eshell-smart-space-goes-to-end t)
            (add-hook 'eshell-mode-hook 'quiescent-eshell-smart-initialise)))

;; This is originally borrowed from Ben's journal, but has since been
;; heavily modified
;; http://www.blogbyben.com/2013/08/a-tiny-eshell-add-on-jump-to-shell.html
(defun quiescent-eshell-switch-to-and-change-dir ()
  "Switch to eshell and make sure we're in the directory the current buffer is in."
  (interactive)
  (let ((dir default-directory))
    (let* ((root               (ignore-errors (project-root (project-current))))
           (eshell-buffer-name (if root (format "*eshell: %s*" root) "*popup-eshell*")))
      (eshell)
      (switch-to-prev-buffer)
      (switch-to-buffer-other-window eshell-buffer-name))
    (goto-char (point-max))
    (unless (eq dir default-directory)
      (cd dir)
      (eshell-send-input)
      (goto-char (point-max)))))

;; Cool prompt from: https://www.reddit.com/r/emacs/comments/6f0rkz/my_fancy_eshell_prompt/
(lambda ()
  (concat
   (propertize "┌─[" 'face `(:foreground "green"))
   (propertize (user-login-name) 'face `(:foreground "red"))
   (propertize "@" 'face `(:foreground "green"))
   (propertize (system-name) 'face `(:foreground "blue"))
   (propertize "]──[" 'face `(:foreground "green"))
   (propertize (format-time-string "%H:%M" (current-time)) 'face `(:foreground "yellow"))
   (propertize "]──[" 'face `(:foreground "green"))
   (propertize (concat (eshell/basename (eshell/pwd))) 'face `(:foreground "white"))
   (propertize "]\n" 'face `(:foreground "green"))
   (propertize "└─>" 'face `(:foreground "green"))
   (propertize (if (= (user-uid) 0) " # " " $ ") 'face `(:foreground "green"))))

;; The following comes from the emacs wiki:
;; https://www.emacswiki.org/emacs/EshellPrompt
(defmacro with-face (str &rest properties)
  "Propertise STR with PROPERTIES."
  `(propertize ,str 'face (list ,@properties)))

(defun shk-eshell-prompt ()
  "A fancy eshell prompt function."
  (let ((header-bg "dark slate grey"))
    (concat
     (with-face (concat (eshell/basename (eshell/pwd)) " ") :background header-bg)
     (with-face (format-time-string "(%Y-%m-%d %H:%M) " (current-time)) :background header-bg :foreground "#888")
     (with-face
      (or (ignore-errors (format "(%s)" (car (vc-git-branches)))) "")
      :background header-bg)
     (with-face "\n" :background header-bg)
     (with-face user-login-name :foreground "#5180b3")
     "@"
     (with-face "localhost" :foreground "green")
     (if (= (user-uid) 0)
         (with-face " #" :foreground "red")
       " $")
     " ")))

(defun quiescent-eshell-prompt ()
  "My eshell prompt.

A combination of one from the wiki and one from reddit."
  (concat
   (with-face "┌─["                                      :foreground "#b2d7ff")
   (with-face (user-login-name)                           :foreground "#5180b3")
   (with-face "@"                                         :foreground "light grey")
   (with-face (system-name)                               :foreground "green")
   (with-face "]──["                                     :foreground "#b2d7ff")
   (with-face (format-time-string "%H:%M" (current-time)) :foreground "light grey")
   (with-face "]──["                                     :foreground "#b2d7ff")
   (with-face (eshell/basename (eshell/pwd))              :foreground "light grey")
   (with-face "]──["                                      :foreground "#b2d7ff")
   (with-face (or (car (vc-git-branches)) "unversioned")  :foreground "light grey")
   (with-face "]\n"                                       :foreground "#b2d7ff")
   (with-face "└->"                                       :foreground "#b2d7ff")
   (with-face (if (= (user-uid) 0) " # " " $ ")           :foreground "light grey")))

(setq eshell-prompt-function 'quiescent-eshell-prompt)
(setq-default eshell-prompt-regexp "└-> \\$ ")

(defun eshell-next-prompt (n)
  "Move to end of Nth next prompt in the buffer.
See `eshell-prompt-regexp'."
  (interactive "p")
  (progn
    (dotimes (_ (abs n))
      (if (> n 0)
          (re-search-forward eshell-prompt-regexp nil t nil)
        (re-search-backward eshell-prompt-regexp nil t nil)))
    (when (< n 0)
      (re-search-forward eshell-prompt-regexp nil t nil))))

(require 'em-hist)

;; 

;; ** EAT (Emulato A Terminal)

(use-package eat
  :straight t)

;; 

;; ** GNUs

(defun quiescent-gnus-summary-toggle-header ()
  "Run summary toggle header."
  (when (null quiescent-starting-up)
    (gnus-summary-toggle-header)))

(add-hook 'gnus-article-mode-hook
          #'quiescent-gnus-summary-toggle-header)

;; 

;; ** Dired

(use-package dired-x
  :config (progn
            (setq dired-omit-mode t)
            (setq dired-omit-files "^\\.[^.]\\|\\.hi$\\|\\.o$\\|\\.exe$")))

(setq dired-dwim-target t)

(use-package dired-subtree
  :straight t
  :config (define-key dired-mode-map (kbd ",") 'dired-subtree-toggle))

(defun quiescent-dired-filter-group-mode ()
  "Enable `dired-filter-group-mode'."
  (when (null quiescent-starting-up)
    (dired-filter-group-mode 1)))

(use-package dired-filter
  :straight t
  :config
  (progn
    (define-key dired-mode-map (kbd "C-c C-'") #'dired-filter-group-mode)
    (add-hook 'dired-mode-hook #'quiescent-dired-filter-group-mode)
    (setq dired-filter-group-saved-groups
          '(("default"
             ("PDF"
              (extension . "pdf"))
             ("LaTeX"
              (extension "tex" "bib"))
             ("Org"
              (extension . "org"))
             ("Archives"
              (extension "zip" "rar" "gz" "bz2" "tar"))
             ("Folders"
              (directory))
             ("Source Code"
              (extension "el" "scala" "rs" "asm"
                         "cl" "lisp" "el" "c"
                         "h" "c++" "h++" "hpp"
                         "hxx" "m" "cc" "cs"
                         "cp" "cpp" "go" "f"
                         "for" "ftn" "f90" "f95"
                         "f03" "f08" "s" "rs"
                         "hi" "hs" "py" "java"
                         "sh" "asd")))))))

(use-package diredfl
  :straight t
  :config
  (progn
    ;; My modifications to the font locking
    (defface quiescent-diredfl-autofile-name
      '((t (:foreground "#3f444a"))) ; Very dark blue
      "*Face used in Dired for names of files that are autofile bookmarks."
      :group 'diredfl)
    (setq diredfl-autofile-name 'quiescent-diredfl-autofile-name)

    (defface quiescent-diredfl-compressed-file-name
      '((t (:foreground "#ECBE7B")))
      "*Face used for compressed file names."
      :group 'diredfl)
    (setq diredfl-compressed-file-name 'quiescent-diredfl-compressed-file-name)

    (defface quiescent-diredfl-compressed-file-suffix
      '((t (:foreground "#905e3c")))
      "*Face used for compressed file suffixes in Dired buffers.
This means the `.' plus the file extension.  Example: `.zip'."
      :group 'diredfl)
    (setq diredfl-compressed-file-suffix 'quiescent-diredfl-compressed-file-suffix)

    (defface quiescent-diredfl-date-time
      '((t (:foreground "#46D9FF")))
      "*Face used for date and time in Dired buffers."
      :group 'diredfl)
    (setq diredfl-date-time 'quiescent-diredfl-date-time)

    (defface quiescent-diredfl-deletion
      '((t (:foreground "#ff6c6b")))
      "*Face used for deletion flags (D) in Dired buffers."
      :group 'diredfl)
    (setq diredfl-deletion 'quiescent-diredfl-deletion)

    (defface quiescent-diredfl-deletion-file-name
      '((t (:foreground "#ff6c6b")))
      "*Face used for names of deleted files in Dired buffers."
      :group 'diredfl)
    (setq diredfl-deletion-file-name 'quiescent-diredfl-deletion-file-name)

    (defface quiescent-diredfl-dir-heading
      '((t (:foreground "#51afef")))
      "*Face used for directory headings in Dired buffers."
      :group 'diredfl)
    (setq diredfl-dir-heading 'quiescent-diredfl-dir-heading)

    (defface quiescent-diredfl-dir-name
      '((t (:foreground "#51afef")))
      "*Face used for directory names."
      :group 'diredfl)
    (setq diredfl-dir-name 'quiescent-diredfl-dir-name)

    (defface quiescent-diredfl-dir-priv
      '((t (:foreground "#51afef")))
      "*Face used for directory privilege indicator (d) in Dired buffers."
      :group 'diredfl)
    (setq diredfl-dir-priv 'quiescent-diredfl-dir-priv)

    (defface quiescent-diredfl-exec-priv
      '((t (:foreground "#98be65")))
      "*Face used for execute privilege indicator (x) in Dired buffers."
      :group 'diredfl)
    (setq diredfl-exec-priv 'quiescent-diredfl-exec-priv)

    ;; For this to show up, you need `F' among the options in `dired-listing-switches'.
    ;; For example, I use "-alF" for `dired-listing-switches'.
    (defface quiescent-diredfl-executable-tag
      '((t (:foreground "#98be65")))
      "*Face used for executable tag (*) on file names in Dired buffers."
      :group 'diredfl)
    (setq diredfl-executable-tag 'quiescent-diredfl-executable-tag)

    (defface quiescent-diredfl-file-name
      '((t (:foreground "#bbc2cf")))
      "*Face used for file names (without suffixes) in Dired buffers.
This means the base name.  It does not include the `.'."
      :group 'diredfl)
    (setq diredfl-file-name 'quiescent-diredfl-file-name)

    (defface quiescent-diredfl-file-suffix
      '((t (:foreground "#7d828d")))
      "*Face used for file suffixes in Dired buffers.
This means the `.' plus the file extension.  Example: `.elc'."
      :group 'diredfl)
    (setq diredfl-file-suffix 'quiescent-diredfl-file-suffix)

    (defface quiescent-diredfl-flag-mark
      '((t (:foreground "#ECBE7B")))
      "*Face used for flags and marks (except D) in Dired buffers."
      :group 'diredfl)
    (setq diredfl-flag-mark 'quiescent-diredfl-flag-mark)

    (defface quiescent-diredfl-flag-mark-line
      '((t (:foreground "#787831311414")))
      "*Face used for flagged and marked lines in Dired buffers."
      :group 'diredfl)
    (setq diredfl-flag-mark-line 'quiescent-diredfl-flag-mark-line)

    (defface quiescent-diredfl-ignored-file-name
      '((t (:foreground "#5B6268")))
      "*Face used for ignored file names  in Dired buffers."
      :group 'diredfl)
    (setq diredfl-ignored-file-name 'quiescent-diredfl-ignored-file-name)

    (defface quiescent-diredfl-link-priv
      '((t (:foreground "#a9a1e1")))
      "*Face used for link privilege indicator (l) in Dired buffers."
      :group 'diredfl)
    (setq diredfl-link-priv 'quiescent-diredfl-link-priv)

    (defface quiescent-diredfl-no-priv
      '((t (:foreground "#bbc2cf")))
      "*Face used for no privilege indicator (-) in Dired buffers."
      :group 'diredfl)
    (setq diredfl-no-priv 'quiescent-diredfl-no-priv)

    (defface quiescent-diredfl-number
      '((t (:foreground "#da8548")))
      "*Face used for numerical fields in Dired buffers.
In particular, inode number, number of hard links, and file size."
      :group 'diredfl)
    (setq diredfl-number 'quiescent-diredfl-number)

    (defface quiescent-diredfl-other-priv
      '((t (:foreground "#c678dd")))
      "*Face used for l,s,S,t,T privilege indicators in Dired buffers."
      :group 'diredfl)
    (setq diredfl-other-priv 'quiescent-diredfl-other-priv)

    (defface quiescent-diredfl-rare-priv
      '((t (:foreground "#bbc2cf")))
      "*Face used for rare privilege indicators (b,c,s,m,p,S) in Dired buffers."
      :group 'diredfl)
    (setq diredfl-rare-priv 'quiescent-diredfl-rare-priv)

    (defface quiescent-diredfl-read-priv
      '((t (:foreground "#ECBE7B")))
      "*Face used for read privilege indicator (w) in Dired buffers."
      :group 'diredfl)
    (setq diredfl-read-priv 'quiescent-diredfl-read-priv)

    (defface quiescent-diredfl-symlink
      '((t (:foreground "#a9a1e1")))
      "*Face used for symbolic links in Dired buffers."
      :group 'diredfl)
    (setq diredfl-symlink 'quiescent-diredfl-symlink)

    (defface quiescent-diredfl-tagged-autofile-name
      '((t (:foreground "#5B6268"))) ; Very pale green
      "*Face used in Dired for names of files that are autofile bookmarks."
      :group 'diredfl)
    (setq diredfl-tagged-autofile-name 'quiescent-diredfl-tagged-autofile-name)

    (defface quiescent-diredfl-write-priv
      '((t (:foreground "#ff6c6b")))
      "*Face used for write privilege indicator (w) in Dired buffers."
      :group 'diredfl)
    (setq diredfl-write-priv 'quiescent-diredfl-write-priv)

    (defconst quiescent-diredfl-font-lock-keywords-1
      (list
       '("^  \\(.+:\\)$" 1 diredfl-dir-heading) ; Directory headers
       '("^  wildcard.*$" 0 'default)       ; Override others, e.g. `l' for `diredfl-other-priv'.
       '("^  (No match).*$" 0 'default)     ; Override others, e.g. `t' for `diredfl-other-priv'.
       '("[^ .]\\(\\.[^. /]+\\)$" 1 diredfl-file-suffix) ; Suffix, including `.'.
       '("\\([^ ]+\\) -> .+$" 1 diredfl-symlink) ; Symbolic links

       ;; 1) Date/time and 2) filename w/o suffix.
       ;;    This is a bear, and it is fragile - Emacs can change `dired-move-to-filename-regexp'.
       `(,dired-move-to-filename-regexp
         (7 diredfl-date-time t t)         ; Date/time, locale (western or eastern)
         (2 diredfl-date-time t t)         ; Date/time, ISO
         (,(concat "\\(.+\\)\\(" (concat (funcall #'regexp-opt diredfl-compressed-extensions)
                                         "\\)[*]?$"))
          nil nil (0 diredfl-compressed-file-name keep t))) ; Compressed-file suffix
       `(,dired-move-to-filename-regexp
         (7 diredfl-date-time t t)         ; Date/time, locale (western or eastern)
         (2 diredfl-date-time t t)         ; Date/time, ISO
         ("\\(.+\\)$" nil nil (0 diredfl-file-name keep t))) ; Filename (not a compressed file)

       ;; Files to ignore
       (list (concat "^  \\(.*\\("
                     (mapconcat #'regexp-quote (or (and (boundp 'dired-omit-extensions)  dired-omit-extensions)
                                                   completion-ignored-extensions)
                                "[*]?\\|")
                     (and diredfl-ignore-compressed-flag
                          (concat "\\|" (mapconcat #'regexp-quote diredfl-compressed-extensions "[*]?\\|")))
                     "[*]?\\)\\)$") ; Allow for executable flag (*).
             1 diredfl-ignored-file-name t)

       ;; Compressed-file (suffix)
       (list (concat "\\(" (concat (funcall #'regexp-opt diredfl-compressed-extensions) "\\)[*]?$"))
             1 diredfl-compressed-file-suffix t)
       '("\\([*]\\)$" 1 diredfl-executable-tag t) ; Executable (*)

       ;; Inode, hard-links, & file size (. and , are for the decimal point, depending on locale)
       ;; See comment for `directory-listing-before-filename-regexp' in `files.el' or `files+.el'.
       '("\\_<\\(\\([0-9]+\\([.,][0-9]+\\)?\\)[BkKMGTPEZY]?[ /]?\\)" 1 'quiescent-diredfl-number)

       ;; Directory names - exclude d:/..., Windows drive letter in a dir heading.
       (list (concat dired-re-maybe-mark dired-re-inode-size "\\(d\\)[^:]")
             '(1 diredfl-dir-priv t) '(".+" (dired-move-to-filename) nil (0 diredfl-dir-name t)))

       (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]........\\(x\\)") ; o x
             '(1 diredfl-exec-priv))
       (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]........\\([lsStT]\\)") ; o misc
             '(1 diredfl-other-priv))
       (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl].......\\(w\\).") ; o w
             '(1 diredfl-write-priv))
       (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]......\\(r\\)..") ; o r
             '(1 diredfl-read-priv))
       (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl].....\\(x\\)...") ; g x
             '(1 diredfl-exec-priv))
       (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl].....\\([lsStT]\\)...") ; g misc
             '(1 diredfl-other-priv))
       (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]....\\(w\\)....") ; g w
             '(1 diredfl-write-priv))
       (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]...\\(r\\).....") ; g r
             '(1 diredfl-read-priv))
       (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]..\\(x\\)...") ; u x
             '(1 diredfl-exec-priv))
       (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]..\\([lsStT]\\)...") ; u misc
             '(1 diredfl-other-priv))
       (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl].\\(w\\)....") ; u w
             '(1 diredfl-write-priv))
       (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]\\(r\\).....") ; u r
             '(1 diredfl-read-priv))

       (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]........\\([-rwxlsStT]\\)") ; o -
             '(1 diredfl-no-priv keep))
       (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl].......\\([-rwxlsStT]\\).") ; g -
             '(1 diredfl-no-priv keep))
       (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]......\\([-rwxlsStT]\\)..") ; u -
             '(1 diredfl-no-priv keep))
       (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl].....\\([-rwxlsStT]\\)...") ; o -
             '(1 diredfl-no-priv keep))
       (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]....\\([-rwxlsStT]\\)....") ; g -
             '(1 diredfl-no-priv keep))
       (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]...\\([-rwxlsStT]\\).....") ; u -
             '(1 diredfl-no-priv keep))
       (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]..\\([-rwxlsStT]\\)......") ; o -
             '(1 diredfl-no-priv keep))
       (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl].\\([-rwxlsStT]\\).......") ; g -
             '(1 diredfl-no-priv keep))
       (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]\\([-rwxlsStT]\\)........") ; u -
             '(1 diredfl-no-priv keep))

       (list (concat dired-re-maybe-mark dired-re-inode-size "\\([bcsmpS]\\)") ; (rare)
             '(1 diredfl-rare-priv keep))
       (list (concat dired-re-maybe-mark dired-re-inode-size "\\(l\\)[-rwxlsStT]") ; l
             '(1 diredfl-rare-priv keep))

       (list (concat "^\\([^\n " (char-to-string dired-del-marker) "].*$\\)")
             '(1 diredfl-flag-mark-line prepend))                          ; Flag/mark lines
       (list (concat "^\\([^\n " (char-to-string dired-del-marker) "]\\)") ; Flags, marks (except D)
             '(1 diredfl-flag-mark prepend))

       (list (concat "^\\([" (char-to-string dired-del-marker) "].*$\\)") ; Deletion-flagged lines
             '(1 diredfl-deletion-file-name prepend))
       (list (concat "^\\([" (char-to-string dired-del-marker) "]\\)") ; Deletion flags (D)
             '(1 diredfl-deletion prepend))))))

(defun quiescent-add-third-level-font-lock ()
  "Add my own font locking afterwards to fix the background."
  (when (null quiescent-starting-up)
    (setq font-lock-defaults
          '((dired-font-lock-keywords
             dired-font-lock-keywords
             diredfl-font-lock-keywords-1
             quiescent-diredfl-font-lock-keywords-1)
            t nil nil beginning-of-line))
    (font-lock-refresh-defaults)))

(add-hook 'diredfl-mode-hook #'quiescent-add-third-level-font-lock)

(diredfl-global-mode 1)

;; 

;; ** EWW

(setq browse-url-browser-function 'eww-browse-url)

;; 

;; ** EPA

(use-package epa-file
  :config (epa-file-enable))

;; ** Org Mode

(use-package all-the-icons
  :straight t)

(defun quiescent-parent-does-not-mention (phrase)
  "Produce pos to skip to if no parent of the subtree item mentions PHRASE."
  (save-excursion
    (let ((found nil))
      (while (and (not found)
                  (condition-case err
                      (progn
                        (when (string-match (format "%s" phrase)
                                            (downcase (buffer-substring-no-properties (point)
                                                                                      (save-excursion (end-of-line)
                                                                                                      (point)))))
                          (setq found t))
                        (outline-up-heading 1)
                        t)
                    (error (setq found (string-match (format "%s" phrase)
                                                     (downcase (buffer-substring-no-properties (point)
                                                                                               (save-excursion (end-of-line)
                                                                                                               (point))))))))))
      (when (not found)
        (condition-case err
            (org-forward-heading-same-level 1)
          (error (goto-char (point-max))))
        (point)))))

(use-package org
  :straight t
  :chords (("xc" . quiescent-org-capture))
  :after all-the-icons
  :config
  (progn
    (require 'ox-latex)
    (setq org-refile-targets
          '((nil :maxlevel . 3)
            (org-agenda-files :maxlevel . 2))
          org-hide-emphasis-markers t
          org-fontify-done-headline t
          org-fontify-whole-heading-line t
          org-fontify-quote-and-verse-blocks t
          org-plantuml-jar-path "~/.jars/plantuml.jar"
          org-ditaa-jar-path "~/.jars/ditaa.jar"
          org-agenda-breadcrumbs-separator " ❱ "
          org-image-actual-width nil
          org-src-fontify-natively t
          org-latex-listings t
          org-clock-persist 'history
          org-clock-persist t
          org-agenda-block-separator (string-to-char " ")
          org-priority-faces '((?A . (:foreground "light gray" :weight bold))
                               (?B . (:foreground "dodger blue"))
                               (?C . (:foreground "OliveDrab")))
          org-agenda-deadline-faces '((1.0 . (:foreground "white"))
                                      (0.5 . (:foreground "orange red"))
                                      (0.0 . (:foreground "dark olive green")))
          org-columns-default-format
          "%25ITEM %TODO %3PRIORITY %TAGS %17Effort(Estimated Effort){:} %CLOCKSUM"
          org-global-properties
          (quote
           (("Effort_ALL" . "0:05 0:15 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00")))
          org-agenda-custom-commands
          (quote
           (("c" "Unscheduled" tags "-SCHEDULED={.+}" nil)
            ("n" "Agenda and all TODOs"
             ((agenda "" nil)
              (alltodo "" nil))
             nil)
            ("o" "My Agenda"
             ((agenda "" ((org-agenda-start-day "+0d")
                          (org-agenda-span 5)
                          (org-agenda-overriding-header "⚡ Agenda:\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
                          (org-agenda-repeating-timestamp-show-all nil)
                          (org-agenda-prefix-format   "  %-3i  %-15b %t%s")
                          (org-agenda-todo-keyword-format "")
                          (org-agenda-current-time-string "<┈┈┈┈┈┈┈ now")
                          (org-agenda-scheduled-leaders '("" ""))
                          (org-agenda-timerange-leaders '(" " " (%d/%d): "))
                          (org-agenda-deadline-leaders '(" ⚡ " " +%3d " " -%2d"))
                          (org-agenda-time-grid (quote ((daily today remove-match)
                                                        (0900 1200 1500 1800 2100)
                                                        "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈")))))))))
          org-agenda-category-icon-alist
          `(("Work" "~/.emacs.d/icons/work.svg" nil nil :ascent center)
            ("Inbox" "~/.emacs.d/icons/checklist.svg" nil nil :ascent center)
            ("Repetative" "~/.emacs.d/icons/loop.svg" nil nil :ascent center)
            ("Games" "~/.emacs.d/icons/walk.svg" nil nil :ascent center)
            ("Chores" "~/.emacs.d/icons/chore.svg" nil nil :ascent center)
            ("Social" "~/.emacs.d/icons/social.svg" nil nil :ascent center)
            ("Writing" "~/.emacs.d/icons/writing.svg" nil nil :ascent center)
            ("Opensource" "~/.emacs.d/icons/github.svg" nil nil :ascent center)
            ("Emacs" "~/.emacs.d/icons/emacs.svg" nil nil :ascent center)
            ("Academics" "~/.emacs.d/icons/exam.svg" nil nil :ascent center)
            ("Finance" "~/.emacs.d/icons/money.svg" nil nil :ascent center)
            ("Improvement" "~/.emacs.d/icons/hammer.svg" nil nil :ascent center)))
    (add-to-list 'org-latex-packages-alist '("" "listings"))
    (add-to-list 'org-latex-packages-alist '("" "color"))
    (org-clock-persistence-insinuate)
    (set-face-foreground 'org-scheduled-today "dodger blue")
    (define-key org-mode-map (kbd "C-M-g") 'org-plot/gnuplot)
    (global-set-key (kbd "C-c C-M-f") #'org-next-block)
    (global-set-key (kbd "C-c C-M-b") #'org-previous-block)
    ;; Override the menu agenda files function because I don't use it
    ;; and I think it's buggy.
    (defun org-install-agenda-files-menu () nil)
    (load-file "~/.emacs.d/conf/additional-org-agendas.el")
    (let ((agenda-files-file (format "%s/agenda-file-list.el" org-directory)))
      (when (file-exists-p agenda-files-file)
        (load-file agenda-files-file)))))

(defun quiescent-org-capture (&optional goto keys)
  "Capture a note to org capture.

Places some context into registers for using while capturing the
note.  I have plans to pick up dates and set the scheduled date
accordingly, but that hasen't been done yet.

Optional arguments GOTO and KEYS are supplied to `org-capture'
itself."
  (interactive)
  (progn
    (copy-to-register ?l
                      (save-excursion (beginning-of-line) (point))
                      (save-excursion (end-of-line)       (point))))
  (org-capture goto keys))

(defvar quiescent-org-reloaded nil
  "Nil if `org-mode' wasn't yet reloaded.")

(with-eval-after-load 'org
  (when (not quiescent-org-reloaded)
    (org-reload)
    (setq quiescent-org-reloaded t)))

(defun quiescent-setup-agenda-line-spacing ()
  "Setup line spacing for my agenda in org mode."
  (setq-local line-spacing 5))

(defun quiescent-activate-hl-line-mode ()
  "Activate `hl-line-mode' in the current buffer."
  (hl-line-mode t))

(add-hook 'org-agenda-mode-hook #'quiescent-setup-agenda-line-spacing)
(add-hook 'org-agenda-mode-hook #'quiescent-activate-hl-line-mode)

(defun quiescent-setup-org-babel-languages()
  "Setup and load the org-babel languages."
  (progn
    (setq org-babel-load-languages
          (quote
           ((emacs-lisp . t)
            (plantuml . t)
            (dot . t)
            (ditaa . t)
            (gnuplot . t)
            (shell . t)
            (python t)
            (calc . t)
            (lisp . t)
            (js . t))))
    (setq org-log-into-drawer 'LOGBOOK)
    (org-babel-do-load-languages
     'org-babel-load-languages org-babel-load-languages)))

(setq org-babel-js-function-wrapper
      "console.log(`${require('util').inspect(function(){\n%s\n}())}`);")

(use-package ob-async
  :straight t)

(add-hook 'after-init-hook #'quiescent-setup-org-babel-languages)

(setq org-capture-templates
      (quote (("w" "Work" entry (file "~/Dropbox/org/work.org")
               "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t))\n%U\n%a\n")
              ("g" "Games" entry (file "~/Dropbox/org/games.org")
               "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t))\n%U\n%a\n")
              ("m" "Maintenance" entry (file "~/Dropbox/org/maintenance.org")
               "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t))\n%U\n%a\n")
              ("e" "Edits" entry (file "~/edits.org")
               "* %?\n%U\n%a\n")
              ("p" "Programming Notes" entry (file "~/notes/programming.gpg")
               "* %?\n%U\n%a\n")
              ("s" "Social" entry (file "~/notes/social.gpg")
               "* %?\n%U\n%a\n")
              ("j" "Journal" entry (file+olp+datetree "~/wip/journal/journal.org")
               "** %<%H:%M> %?\n"))))

(define-key org-mode-map (kbd "M-i") #'org-goto)

(advice-add #'org-goto :before #'push-mark)

(define-key org-mode-map (kbd "C-c C--") #'q-insert-checkbox)

(use-package ob-gnuplot)

(use-package ob-shell)

(use-package ob-python)

(use-package ob-mermaid
  :straight t)

(use-package cider
  :straight t
  :demand t)

(use-package ob-clojure
  :init (setq org-babel-clojure-backend 'cider))

(defun quiescent-render-html--overlays (beg end)
  "Get the overlays in region [BEG, END) which are used by `quiescent-render-html'."
  (cl-remove-if-not (lambda (overlay) (overlay-get overlay 'render-html))
                    (overlays-in beg end)))

(defun quiescent-render-html-ob-export ()
  "Renders all html output blocks in the current buffer.

Uses `shr-render-buffer' to render the block.  When the point
enters the region then it reveals the original contents.

The original text is stored in an overlay property and revealed
when the point enters the region using the post command hook:
`quiescent--org-pretty-render-post-command-hook'."
  (interactive)
  (save-window-excursion
    (quiescent-render-html-register-hook)
    (goto-char (point-min))
    (quiescent-render-html-unrender-all)
    (while (search-forward "#+begin_export html" nil t)
      (let* ((start         (progn (beginning-of-line) (point)))
             (start-content (progn (end-of-line) (forward-char 1) (point)))
             (end           (progn (goto-char (org-babel-result-end)) (point)))
             (end-content   (progn (forward-line -1) (point)))
             (overlays      (quiescent-render-html--overlays (point-min) (point-max))))
        (mapc #'delete-overlay overlays)
        (let* ((original-content (buffer-substring start-content end-content))
               (rendered-content (with-temp-buffer
                                   (insert original-content)
                                   (shr-render-buffer (current-buffer))
                                   (buffer-substring (point-min) (point-max)))))
          (quiescent-render-html-insert-rendered (make-overlay start end)
                                                 (buffer-substring start end)
                                                 rendered-content))))))

(defun quiescent-render-html-insert-rendered (overlay &optional original-content rendered-content)
  "Insert the rendered content for OVERLAY.

Optionally supply the original and rendered content, used when
first created to remember those values."
  (let ((start    (overlay-start overlay))
        (end      (overlay-end   overlay))
        (original (or original-content (overlay-get overlay 'original)))
        (rendered (or rendered-content (overlay-get overlay 'rendered))))
    (delete-overlay overlay)
    (delete-region start end)
    (goto-char start)
    (insert rendered)
    (let ((new-overlay (make-overlay start (+ start (length rendered)))))
      (overlay-put new-overlay 'face      'variable-pitch)
      (overlay-put new-overlay 'evaporate t)
      (overlay-put new-overlay 'original original)
      (overlay-put new-overlay 'rendered rendered)
      (overlay-put new-overlay 'render-html  t)
      (overlay-put new-overlay 'showing-html nil))))

(defun quiescent-render-html-insert-original (overlay)
  "Insert the original content for OVERLAY."
  (let ((start    (overlay-start overlay))
        (end      (overlay-end   overlay))
        (original (overlay-get   overlay 'original))
        (rendered (overlay-get   overlay 'rendered)))
    (delete-overlay overlay)
    (delete-region start end)
    (goto-char start)
    (insert original)
    (let ((new-overlay (make-overlay start (+ start (length original)))))
      (overlay-put new-overlay 'evaporate t)
      (overlay-put new-overlay 'original original)
      (overlay-put new-overlay 'rendered rendered)
      (overlay-put new-overlay 'render-html  t)
      (overlay-put new-overlay 'showing-html t))))

(defun quiescent-render-html-post-command-hook ()
  "Reveal the original contents when a point enters rendered blocks."
  (when (and (not (member last-command '(kill-region end-of-buffer)))
             (not (region-active-p)))
    (let ((current-point (point)))
      (progn
        (let ((overlays (quiescent-render-html--overlays (point-min) (point-max))))
          (dolist (overlay overlays)
            (when (and (overlay-get overlay 'showing-html)
                       (or (>= current-point (overlay-end overlay))
                           (<  current-point (overlay-start overlay))))
              (save-excursion (quiescent-render-html-insert-rendered overlay)))))
        (let* ((overlays-here (quiescent-render-html--overlays (point) (point)))
               (overlay       (car overlays-here)))
          (when (and (not (null overlay))
                     (null (overlay-get overlay 'showing-html)))
            (save-excursion (quiescent-render-html-insert-original overlay))))))))

(defun quiescent-render-html-unrender-all ()
  "Display source html for all render-able result blocks in this file."
  (interactive)
  (dolist (overlay (quiescent-render-html--overlays (point-min) (point-max)))
    (save-excursion
      (ignore-errors
        (quiescent-render-html-insert-original overlay)))))

;; Live re-render HTML from org outputs

;; (make-variable-buffer-local 'post-command-hook)

;; (make-variable-buffer-local 'before-save-hook)

;; (defun quiescent-render-html-register-hook ()
;;   "Register the post command hook for rendering html."
;;   (add-hook 'post-command-hook #'quiescent-render-html-post-command-hook)
;;   (add-hook 'before-save-hook  #'quiescent-render-html-unrender-all))

(defun quiescent-clear-org-results ()
  "Clear all org results in this buffer."
  (interactive)
  (save-excursion
    (remove-hook 'post-command-hook #'quiescent-render-html-post-command-hook)
    (quiescent-render-html-unrender-all)
    (goto-char (point-min))
    (while (re-search-forward "^#\\+RESULTS:" nil t)
      (beginning-of-line)
      (delete-region (point) (org-babel-result-end)))
    (add-hook 'post-command-hook #'quiescent-render-html-post-command-hook)))

(use-package org-bullets
  :straight t
  :config
  (add-hook 'org-mode-hook #'quiescent-activate-org-bullets-mode)
  (defun quiescent-activate-org-bullets-mode ()
    (when (null quiescent-starting-up)
      (org-bullets-mode 1))))

(setq org-enforce-todo-dependencies t)

(use-package ox-reveal
  :straight t
  :demand t)

(require 'url)

(defun quiescent-download-reveal-js-here ()
  "Download the reveal source file here."
  (interactive)
  (progn
    (url-copy-file "https://github.com/hakimel/reveal.js/archive/3.9.2.tar.gz" "reveal.js-3.9.2.tar.gz")
    (shell-command "tar -xzvf reveal.js-3.9.2.tar.gz")
    (shell-command "mv reveal.js-3.9.2/ reveal.js")))

;; 

;; ** Secretaria

(use-package alert
  :straight t)

(use-package secretaria
  :straight t)

(copy-face 'fringe 'quiescent-alert-saved-fringe-face)

(defun quiescent-alert-fringe-notify (info)
  (progn
    (set-face-background 'fringe (cdr (assq (plist-get info :severity)
                                            alert-severity-colors)))
    (message "%s" (plist-get info :message))))

(defun quiescent-alert-fringe-restore (_info)
  (copy-face 'quiescent-alert-saved-fringe-face 'fringe))

(alert-define-style 'fringe-message :title "Change the fringe color"
                    :notifier #'quiescent-alert-fringe-notify
                    :remover #'quiescent-alert-fringe-restore)

(defvar *quiescent-reminder-timer* nil
  "The timer for the current reminder.")

(defun quiescent-setup-timed-reminder (reminder seconds)
  "Fires a REMINDER every SECONDS."
  (interactive "sReminder: \nnSeconds: ")
  (progn
    (when *quiescent-reminder-timer*
      (cancel-timer *quiescent-reminder-timer*))
    (setq *quiescent-reminder-timer*
          (run-at-time
           seconds
           seconds
           (lambda () (alert reminder
                             :title "Reminder"
                             :severity 'trivial
                             :style 'fringe-message))))))

(defun quiescent-cancel-timed-reminder ()
  "Cancel any timed reminder or do nothing if there is none."
  (interactive)
  (if *quiescent-reminder-timer*
      (progn
        (cancel-timer *quiescent-reminder-timer*)
        (setq *quiescent-reminder-timer* nil)
        (message "Cancelled timed reminder."))
    (error "No timed reminder running")))

(defun org-archive-done-tasks ()
  "Archive all tasks marked DONE in the current subtree."
  (interactive)
  (mapc (lambda (entry)
          (goto-char entry)
          (org-archive-subtree))
        (reverse (org-map-entries (lambda () (point)) "TODO=\"DONE\"" 'tree))))

;; 

;; ** Demo It

(use-package demo-it
  :straight t)

;; 

;; Magit

(use-package magit
  :straight t
  :custom (magit-diff-refine-hunk t)
  :config
  (progn
    (setenv "GIT_ASKPASS" "git-gui--askpass")
    (global-set-key (kbd "C-c m") #'magit-status)))

;; 

;; ** Git Related Navigation

;; Source: https://macroexpand.net/pages/git-related.html

(use-package git-related
  :load-path "~/.emacs.d/lisp")

;; 

;; ** Git Time Machine

;; Note: moved to: https://codeberg.org/pidu/git-timemachine.git
(use-package git-timemachine
  :straight t)

;; 

;; ** Chess

(use-package chess
  :straight t)

;; 

;; ** Lock this PC

(defun quiescent-lock-pc ()
  "Lock this computer.

Enter the password for this account to unlock it."
  (interactive)
  (start-process "slock" "*slock-buffer*" "slock"))

;; 

;; ** GNU Plot

(use-package gnuplot
  :straight t)

;; 

;; ** Rest Client

(use-package restclient
  :straight t
  :config (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))

;; 

;; ** Coffee Business

(defun quiescent-add-coffee-order (date amount description deliver-by)
  "Add an order made on DATE costing AMOUNT for coffee described by DESCRIPTION.

The cofee should be delivered by DELIVER-BY."
  (interactive
   (list
    (with-temp-buffer
      (org-time-stamp nil)
      (let* ((stamp (buffer-substring (point-min) (point-max)))
             (parts (split-string stamp "[^0-9]+" t)))
        (pcase parts
          (`(,y ,m ,d) (format "%s/%s/%s" d m y)))))
    (read-number "Cost: ")
    (read-string "Description: ")
    (with-temp-buffer
      (org-time-stamp nil)
      (let* ((stamp (buffer-substring (point-min) (point-max)))
             (parts (split-string stamp "[^0-9]+" t)))
        (pcase parts
          (`(,y ,m ,d) (format "%s/%s/%s" d m y)))))))
  (save-window-excursion
    (find-file "~/Dropbox/work/coffee/orders.org")
    (goto-char (point-min))
    (search-forward "#+TBLFM")
    (beginning-of-line)
    (forward-line -2)
    (insert (format "| %s | %s | %s | %s | nil | nil | nil |\n"
                    date amount description deliver-by))
    (org-cycle)))

(require 'cl-lib)

;; 

;; ** IA Writer Mode

(use-package writeroom-mode
  :straight t)

(defvar ia-writer-highlight-current-sentance-delay 0.1
  "The number of seconds to wait before highlighting the current sentance.")

(define-minor-mode ia-writer-mode
  "An 'iA Writer' like mode."
  :init-value nil
  :lighter " iA Writer"
  :keymap nil
  :group 'ia-writer
  (if ia-writer-mode
      (progn
        (writeroom-mode 1)
        (blink-cursor-mode -1)
        (visible-mark-mode -1)
        (auto-fill-mode 1)
        (run-at-time ia-writer-highlight-current-sentance-delay
                     nil
                     (ia-writer-highlight-current-sentance-on-idle-timer-exirey
                      (current-buffer))))
    (progn
      (writeroom-mode -1)
      (blink-cursor-mode 1)
      (visible-mark-mode 1)
      (auto-fill-mode -1)
      (mapc #'delete-overlay (ia-writer--overlays)))))

(defun ia-writer--overlays ()
  "Get the overlays in the current buffer which are used by `ia-writer-mode'."
  (cl-remove-if-not (lambda (overlay) (overlay-get overlay 'hidden))
                    (overlays-in (point-min) (point-max))))

(defvar ia-writer-hidden-font-colour "#666168"
  "The colour of the font which should fade into the background.")

(defun ia-writer-set-light-mode ()
  "Set the background colour of the overlays for a light-mode theme."
  (interactive)
  (setq ia-writer-hidden-font-colour "#e1dfe1"))

(defun ia-writer-highlight-current-sentance-on-idle-timer-exirey (buffer)
  "Highlight the at point when the idle timer expires.

  BUFFER is the buffer to do the highlighting in.  If that buffer
  is gone or doesn't have the right mode then cancel the timer."
  (lambda () (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when ia-writer-mode
                   (let* ((start    (save-excursion (backward-sentence) (point)))
                          (end      (save-excursion (ignore-errors (forward-sentence))  (point)))
                          (overlays (ia-writer--overlays))
                          (first    (or (and overlays (move-overlay (car overlays) (point-min) start))
                                        (make-overlay (point-min) start)))
                          (second   (or (and overlays (move-overlay (cadr overlays) end (point-max)))
                                        (make-overlay end         (point-max)))))
                     (when (not overlays)
                       (overlay-put first  'hidden t)
                       (overlay-put first  'face   `(:foreground ,ia-writer-hidden-font-colour))
                       (overlay-put second 'hidden t)
                       (overlay-put second 'face  `(:foreground ,ia-writer-hidden-font-colour))))
                   (run-at-time ia-writer-highlight-current-sentance-delay
                                nil
                                (ia-writer-highlight-current-sentance-on-idle-timer-exirey buffer)))))))

;; 

;; ** AucTeX

(require 'ox-latex)

(setq TeX-engine 'default)

(setq org-latex-listings t)

(ignore-errors
  (unless (package-installed-p 'auctex)
    (list-packages)
    (package-install 'auctex)))

(defun quiescent-disable-yas-minor-mode ()
  "Disable YAS minor mode."
  (yas-minor-mode -1))

(setq TeX-view-program-selection
      '(((output-dvi has-no-display-manager)
         "dvi2tty")
        ((output-dvi style-pstricks)
         "dvips and gv")
        (output-dvi "xdvi")
        (output-pdf "EmacsClient")
        (output-html "xdg-open")))

(setq TeX-view-program-list '(("EmacsClient" "emacsclient %o")))

(defun quiescent-export-org-to-pdf-via-latex-asynch ()
  "Export the current document to a PDF via LaTeX asynchronously."
  (interactive)
  (org-latex-export-to-pdf t))

(define-key org-mode-map (kbd "s-e") #'quiescent-export-org-to-pdf-via-latex-asynch)

;; 

;; ** PDF Tools

;; (defvar install-pdf-tools
;;   '(use-package pdf-tools
;;      :straight t
;;      :hook (pdf-view-mode . quiescent-disable-composable-mode)
;;      :demand t
;;      :config (progn
;;                (define-key pdf-view-mode-map (kbd "M-w") #'pdf-view-kill-ring-save)
;;                (pdf-tools-install))))

;; (eval install-pdf-tools)

;; (defun quiescent-disable-composable-mode ()
;;   "Disable composable mode."
;;   (when (null quiescent-starting-up)
;;     (progn
;;    (composable-mode -1)
;;    (composable-mark-mode -1))))

;; 

;; Reload custom in case anything overwrote it
(load custom-file)

;; * DONE!

(setq quiescent-starting-up nil)
(put 'narrow-to-region 'disabled nil)
