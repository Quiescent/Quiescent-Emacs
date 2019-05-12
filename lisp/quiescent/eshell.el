;;; quiescent/eshell --- Setup, convenience functions and configuration for eshell -*- lexical-binding: t; -*-

;;; Commentary:
;; Jump around in eshell based on the most frequently used stuff.
;; Look up the fasd command.
;; https://developer.mozilla.org/en-US/docs/Mozilla/Tech/Places/Frecency_algorithm

;;; Code:

(require 'use-package)
(eval-and-compile
  (when (package-installed-p 'use-package-chords)
    (package-activate 'use-package-chords))
  (use-package use-package-chords
      :ensure t))
(require 'eshell)
(require 'em-prompt)
(require 'vc-git)

(defconst quiescent-eshell-project-buffer-regexp "\*eshell .*\*"
  "A regular expression which matches project eshell buffer names.")

(defun quiescent-eshell ()
  "Create split current window and make bottom half an eshell instance."
  (interactive)
  (if (or (string-equal (buffer-name) eshell-buffer-name)
          (string-match quiescent-eshell-project-buffer-regexp (buffer-name)))
      (delete-window)
      (quiescent-eshell-switch-to-and-change-dir)))

(use-package em-smart
    :chords ((",s" . quiescent-eshell))
    :config (progn
              (setq eshell-where-to-jump 'begin)
              (setq eshell-review-quick-commands nil)
              (setq eshell-smart-space-goes-to-end t)
              (add-hook 'eshell-mode-hook 'eshell-smart-initialize)))

;; This is originally borrowed from Ben's journal, but has since been
;; heavily modified
;; http://www.blogbyben.com/2013/08/a-tiny-eshell-add-on-jump-to-shell.html
(defun quiescent-eshell-switch-to-and-change-dir ()
  "Switch to eshell and make sure we're in the directory the current buffer is in."
  (interactive)
  (let ((dir default-directory))
    ;; TODO create a project shell function
    (pop-to-buffer "*popup-eshell*")
    (eshell)
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
   (propertize (concat (eshell/pwd)) 'face `(:foreground "white"))
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
     (with-face (concat (eshell/pwd) " ") :background header-bg)
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
   (with-face "┌─["                                       :foreground "#b2d7ff")
   (with-face (user-login-name)                           :foreground "#5180b3")
   (with-face "@"                                         :foreground "light grey")
   (with-face (system-name)                               :foreground "green")
   (with-face "]──["                                      :foreground "#b2d7ff")
   (with-face (format-time-string "%H:%M" (current-time)) :foreground "light grey")
   (with-face "]──["                                      :foreground "#b2d7ff")
   (with-face (eshell/pwd)                                :foreground "light grey")
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

(provide 'quiescent/eshell)
;;; eshell ends here
