;;; system-vars -- A collection of variables which control which parts
;;; of my Emacs are active per machine.

;;; Commentary:

;; On my Mac:
;; To get aspell working as the back end for flyspell-mode I had to make a symbolic link:
;;   `sudo ln -s /usr/local/bin/aspell /usr/bin/aspell'

;; A simple list of variables listed here which are specific to this
;; particular computer.  I use these variables to control switches
;; which are only applicable to Windows and specific default
;; configuration.

;;; Code:

(defvar quiescent-work-machine nil
  "Whether I'm on my work machine..")

(defvar quiescent-exwm-machine nil
  "Whether this machine uses EXWM as it's window manager.")

(defvar quiescent-exwm-multiple-monitors nil
  "Whether this machine uses EXWM and has multiple monitors.")

(defvar quiescent-home-pc-linux nil
  "Whether this computer is my Home Linux PC.")

(defvar quiescent-macbook nil
  "Whether this computer is my macbook.")

(defun quiescent-computer-with-langtool-installed-p ()
  "Produce t if this computer is using langtool."
  (or quiescent-macbook))

(provide 'system-vars)
;;; system-vars.el ends here
