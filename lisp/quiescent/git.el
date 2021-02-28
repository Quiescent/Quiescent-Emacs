;;; git --- functions for use in git, especially via eshell -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'vc-git)

(defun quiescent-current-branch-name ()
  "Produce the current branch name."
  (car (vc-git-branches)))

(defun quiescent-insert-current-branch-name ()
  "Print the current branch name at point."
  (interactive)
  (insert (quiescent-current-branch-name)))

(require 'esh-mode)

(defun quiescent-add-insert-branch-binding-eshell ()
  "Add the insert branch binding in eshell mode."
  (when (null quiescent-starting-up)
    (define-key eshell-mode-map (kbd "C-c b") #'quiescent-insert-current-branch-name)))

(add-hook 'eshell-mode-hook #'quiescent-add-insert-branch-binding-eshell)

(defconst q-git-log-entry-parse-up-to-extra-details ".*Author: \\(.*\\)\nDate:   \\(.*\\)\n\n    \\(.*\\)$"
  "A regular expression for parsing git log entries.")

(defconst q-git-month-translation-alist '(("Jan" . "01")
                                          ("Feb" . "02")
                                          ("Mar" . "03")
                                          ("Apr" . "04")
                                          ("May" . "05")
                                          ("Jun" . "06")
                                          ("Jul" . "07")
                                          ("Aug" . "08")
                                          ("Sep" . "09")
                                          ("Oct" . "10")
                                          ("Nov" . "11")
                                          ("Dec" . "12"))
  "A transaltion alist for string months to ints.")

(defun q-git-to-org-time-stamp (git-time-stamp)
  "Convert GIT-TIME-STAMP into org format for time stamps."
  (cl-labels ((month-to-int (month)
                            (cdr (assoc month q-git-month-translation-alist))))
    (pcase (split-string git-time-stamp " " t)
      (`(,_ ,month ,day ,time ,year ,_)
       (format "%s-%s-%02d %s" year (month-to-int month) (string-to-number day) time)))))

(defun q-git-parse-log-entry (log-entry-text)
  "Parse LOG-ENTRY-TEXT into a list structure."
  (progn
    (string-match q-git-log-entry-parse-up-to-extra-details log-entry-text)
    (let ((author  (substring log-entry-text (match-beginning 1) (match-end 1)))
          (date    (substring log-entry-text (match-beginning 2) (match-end 2)))
          (summary (substring log-entry-text (match-beginning 3) (match-end 3)))
          (detail  (substring log-entry-text (match-end 3))))
      `(GIT-LOG ,author ,(q-git-to-org-time-stamp  date) ,summary ,detail))))

(defun q-git-parse-log (log-lines)
  "Parse LOG-LINES into a list of list structures."
  (mapcar #'q-git-parse-log-entry (split-string log-lines "^commit" t "[ \n]")))

(defun q-grab-match (regex string &optional match-group)
  "Match REGEX to STRING and then produce the matching text.
If MATCH-GROUP is supplied then produce that matching group."
  (progn
    (when (null match-group)
      (setq match-group 0))
    (when (string-match regex string)
      (substring string (match-beginning match-group) (match-end match-group)))))

(defun q-push-hash (key value table)
  "Define KEY to be a list updated with VALUE from the current list in TABLE.

Creates a list with only value in it if there isn't one yet."
  (puthash key (append (list value) (gethash key table)) table))

(defvar *q-git-augment-task* #'identity
  "A function to augment a task entry when grouping by commiter then task.")

(defun q-parse-string-field-by-regexp (regexp)
  "Parse a string field from the current buffer using the first hit of REGEXP."
  (q-grab-match regexp
                (buffer-string)
                1))

(defvar *q-git-report-buffer* "*git-report*"
  "The name of the buffer to wirte the report to.")

(defvar *q-git-grouping-function* #'q-git-group-by-commiter-then-task
  "The function which will perform grouping prior to writing the report.")
(defvar *q-git-printing-function* #'identity
  "The function which will print the report.
Must be compatible with the output of the grouping function.")

(defvar quiescent-patch-file-buffer-name "*patch-files*"
  "The name of buffer to create a patch files in.")

(defun quiescent-bad-commit-message (patch-file-path)
  "Produce a list of the lines where bad commits are found.

Search the file at PATCH-FILE-PATH for the invalid messages."
  (with-temp-buffer
    (let (start
          end
          bad)
      (insert-file-contents patch-file-path)
      (while (re-search-forward "Subject: \\[PATCH\\( [0-9]+/[0-9]+\\|\\)\\] " nil t nil)
        (progn
          (setq start (point))
          (setq end   (re-search-forward "^---$"))
          (narrow-to-region start end)
          (goto-char (point-min))
          (unfill-paragraph)
          (when (re-search-forward ".\\{79\\}" nil t nil)
            (widen)
            (push (line-number-at-pos) bad))
          (widen)))
      bad)))

(defun quiescent-bad-commits-for-patches-in-dir (dir)
  "Produce a patches in DIR which have bad commit messages.

A bad commit is defined as having more than 78 characters in any
of it's lines."
  (interactive "DDirectory: ")
  (let ((patch-files (directory-files dir nil "\\.patch$" t)))
    (progn
      (switch-to-buffer quiescent-patch-file-buffer-name)
      (erase-buffer)
      (dolist (patch-file patch-files)
        (let ((bad-lines (quiescent-bad-commit-message patch-file)))
          (when bad-lines
            (insert patch-file "\n - " (mapconcat #'number-to-string bad-lines "\n - ") "\n")))))))

(defun quiescent-parse-month (month)
  "Parse MONTH into it's integer representation."
  (pcase (downcase month)
    ("jan" 1)
    ("feb" 2)
    ("mar" 3)
    ("apr" 4)
    ("may" 5)
    ("jun" 6)
    ("jul" 7)
    ("aug" 8)
    ("sep" 9)
    ("oct" 10)
    ("nov" 11)
    ("dec" 12)))

(defun quiescent-parse-git-time-string (s)
  "Parse S into an EMACS time list (SEC MINUTE HOUR DAY MONTH YEAR DOW DST UTCOFF)."
  (pcase (split-string s " " t)
    (`(,_ ,month ,day ,_ ,year)
     `(0 0 0 ,(string-to-number day) ,(quiescent-parse-month month) ,(string-to-number year) 0 nil 0))))

(use-package git-timemachine
  :straight t)

(provide 'quiescent/git)
;;; quiescent/git ends here
