;;; quiescent/maven --- Functions for convenience while working with maven -*- lexical-binding: t; -*-

;;; Commentary:

;; If I'm writing a spring boot app in the future then the following
;; commands can be used to run it with profiles (local and default)
;; and to debug it with those same profiles respectively:
;; - mvn-backend spring-boot:run -Drun.profiles=local,default
;; - mvn-backend spring-boot:run -Drun.profiles=local,default -Drun.jvmArguments="-agentlib:jdwp=transport=dt_shmem,address=jdbconn,server=y,suspend=n"
;;
;; Attaching to a debug process in java is done as follows:
;;  - jdb -attach <server>

;;; Code:

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

(provide 'quiescent/maven)
;;; maven ends here
