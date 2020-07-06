;;; bare-init --- A bare bones startup of emacs for use in compilation.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'package)

(setq package-archives
      (quote (("gnu"   . "http://elpa.gnu.org/packages/")
              ("melpa" . "http://melpa.org/packages/"))))

(load-file "~/.emacs.d/conf/system-vars.el")
(if (file-exists-p "~/.emacs.d/conf/system-conf.el")
    (load-file "~/.emacs.d/conf/system-conf.el")
  (warn "No system-conf.el file exists!  Please configure variables in ~/.emacs.d/conf/system-vars.el and put them in ~/.emacs.d/conf/system-conf.el"))

(package-initialize)
(package-refresh-contents)
(unless (package-installed-p 'use-package)
  (package-install 'use-package nil))
(unless (package-installed-p 'use-package-chords)
  (package-install 'use-package-chords nil))

(require 'use-package-chords)

;; Required for org-brain export.  It's not a proper package yet.
(use-package xmlgen
  :ensure t)
(use-package a
  :ensure t)

(add-to-list 'load-path "~/.emacs.d/lisp")

(provide 'bare-init)
;;; bare-init ends here
