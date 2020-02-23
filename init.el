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

;; Custom variables etc.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

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
  (setq quiescent-starting-up nil))
