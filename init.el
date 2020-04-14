;;; .emacs --- My emacs root config file.

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

;; Send this to the dude on the mailing list:
;; (defcustom semantic-lex-spp-macro-max-length-to-save 200
;; "Maximum length of an SPP macro before we opt to not save it."
;; :type 'integer
;; :group 'semantic)

;; For debugging what gets compiled at startup
;;(debug-on-entry #'byte-compile)

(setq epa-pinentry-mode 'loopback)

;; Custom variables etc.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(when quiescent-exwm-machine
  (use-package xelb
    :ensure t)
  (use-package exwm
    :ensure t
    :init (when quiescent-exwm-multiple-monitors
            ;; From https://github.com/ch11ng/exwm/wiki
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
    (require 'exwm-config)
    (defun exwm-config-ido ()
      "Override the config function for ido to do nothing since I don't use it."
      nil)
    (exwm-config-default)
    (fringe-mode 15)
    (exwm-input-set-key (kbd "C-c C-j") #'exwm-input-grab-keyboard)
    (exwm-input-set-key (kbd "s-z")     #'isearchb-activate)))

(add-to-list 'load-path "~/.emacs.d/conf")
(require 'system-vars)

(unwind-protect
    (progn
      (let ((file-name-handler-alist nil))

        (add-to-list 'load-path (concat user-emacs-directory "lisp"))
        (require 'org)
        (org-reload) ;; This forces the overriden org to be loaded

        (let* ((org-babel-use-quick-and-dirty-noweb-expansion t)
               (org-babel-noweb-error-all-langs t)
               (default-directory "~/.emacs.d")
               (config-source (expand-file-name "startup.org"
                                                user-emacs-directory))
               (config-tangled (expand-file-name "startup.el"
                                                 user-emacs-directory))
               (config-compiled (expand-file-name "startup.elc"
                                                  user-emacs-directory)))
          (when (or (not (file-exists-p config-tangled))
                    (file-newer-than-file-p config-source config-tangled))
            (org-babel-tangle-file config-source config-tangled 'emacs-lisp))
          (when (null (byte-recompile-file config-tangled nil 0))
            (error "Compilation errors"))
          (load-file config-compiled)))
      (put 'narrow-to-region 'disabled nil)
      (put 'scroll-left 'disabled nil))
  (setq quiescent-starting-up nil)
  (when (eq system-type 'darwin)
    (set-face-attribute 'default nil :height 115)))
