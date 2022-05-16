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

(add-to-list 'load-path "~/.emacs.d/conf")
(require 'system-vars)
(require 'cl-lib)

(use-package all-the-icons
  :straight t)

(use-package org
  :straight t
  :chords (("xc" . quiescent-org-capture))
  :after all-the-icons
  :config
  (progn
    (let ((agenda-files-file (format "%s/agenda-file-list.el" org-directory)))
      (when (file-exists-p agenda-files-file)
        (load-file agenda-files-file)))
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
    (defun org-install-agenda-files-menu () nil)))

(unwind-protect
    (progn
      (let ((file-name-handler-alist nil))

        (add-to-list 'load-path (concat user-emacs-directory "lisp"))
        (require 'org)
        (org-reload) ;; This forces the overriden org to be loaded

        (let* ((org-babel-use-quick-and-dirty-noweb-expansion t)
               (org-babel-noweb-error-all-langs t)
               (default-directory "~/.emacs.d")
               (build-dir        (concat user-emacs-directory "build/" ))
               (config-source    (expand-file-name "startup.org"
                                                   user-emacs-directory))
               (config-deps      (expand-file-name "startup-deps.el" build-dir))
               (config-tangled   (expand-file-name "startup.el"
                                                   user-emacs-directory))
               (config-extracted (expand-file-name "startup.el" build-dir))
               (config-compiled  (expand-file-name "startup.elc" build-dir))
               (native-compilation-available (and (fboundp 'native-comp-available-p)
                                                  (native-comp-available-p))))
          (add-to-list 'load-path build-dir)
          (when (or (not (file-exists-p config-tangled))
                    (file-newer-than-file-p config-source config-tangled))
            ;; Problem is that write-region seems to write the unicode
            ;; chars escaped instead of evaluated...
            (let ((coding-system-for-write 'utf-8))
              (org-babel-tangle-file config-source config-tangled "emacs-lisp"))
            (let* ((print-level   nil)
                   (print-length  nil)
                   (startup-forms (save-window-excursion
                                    (with-temp-buffer
                                      (find-file config-tangled)
                                      (cl-remove-if-not #'listp (car (read-from-string (format "(%s)" (buffer-substring (point-min) (point-max)))))))))
                   (dependencies  (cl-remove-if-not (lambda (form) (eq 'use-package (car form))) startup-forms)))
              (save-window-excursion
                (find-file config-deps)
                (kill-region (point-min) (point-max))
                (insert ";;; -*- lexical-binding: t -*-\n\n")
                (insert (format "(require 'system-vars)\n"))
                (mapc (lambda (form) (print form (current-buffer))) dependencies)
                (save-buffer))
              (load-file config-deps)))
          (when (or (not (file-exists-p config-compiled))
                    (file-newer-than-file-p config-extracted config-compiled))
            ;; Problem is that write-region seems to write the unicode
            ;; chars escaped instead of evaluated...
            (let ((coding-system-for-write 'utf-8))
              (org-babel-tangle-file config-source config-tangled "emacs-lisp"))
            (let* ((print-level   nil)
                   (print-length  nil)
                   (startup-forms (save-window-excursion
                                    (with-temp-buffer
                                      (find-file config-tangled)
                                      (cl-remove-if-not #'listp (car (read-from-string (format "(%s)" (buffer-substring (point-min) (point-max)))))))))
                   (non-deps      (cl-remove-if     (lambda (form) (eq 'use-package (car form))) startup-forms)))
              (save-window-excursion
                (find-file config-extracted)
                (kill-region (point-min) (point-max))
                (insert ";;; -*- lexical-binding: t -*-\n\n")
                (insert (format "(require 'system-vars)\n"))
                (mapc (lambda (form) (print form (current-buffer))) non-deps)
                (insert "\n(provide 'startup)")
                (save-buffer))
              (load-file config-deps)
              (byte-compile-file config-extracted)))
          (org-reload)
          (load-file config-deps)
          (load-file config-compiled)
          (require 'startup)))
      (put 'narrow-to-region 'disabled nil)
      (put 'scroll-left 'disabled nil))
  (setq quiescent-starting-up nil)
  (when (eq system-type 'darwin)
    (set-face-attribute 'default nil :height 115)))
