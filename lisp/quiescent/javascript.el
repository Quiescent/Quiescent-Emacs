;;; quiescent/javascript --- Setup for editing javascript -*- lexical-binding: t; -*-

;;; Commentary:
;; Linters used in conjunction with flycheck (install them with npm
;; install -g):
;;
;;  - jshint
;;  - eslint
;;  - jscs

;; jscs needs a config file like so:
;; {
;;     "preset": "google",
;;     "requireCurlyBraces": null
;; }

;; Debuggers require a global install of:
;;  - babel: npm i -g babel-cli
;;  - jest: npm i -g jest
;;  - mocha: npm i -g mocha

;;; Code:

(require 'quiescent/haskell)

(require 'use-package)

(use-package js2-mode
  :ensure t
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
            (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
            (define-key js2-mode-map (kbd "<f5>") #'quiescent-start-node-with-this-file)
            (define-key js2-mode-map (kbd "<f6>") #'quiescent-start-mocha-with-this-file)
            (define-key js2-mode-map (kbd "<f7>") #'quiescent-start-jest-with-this-file)))

(require 'flycheck)

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

(defun quiescent-select-jshint ()
  "Select JSHint as the linter in this buffer."
  (interactive)
  (when (null quiescent-starting-up)
    (flycheck-select-checker 'javascript-jshint)))

(defconst quiescent-node-url-extracting-regexp "://.*127.0.0.1:\\([0-9]+\\)/\\(.*\\)"
  "A regular expression which can extract the port and path of the node process for indium.")

(defconst quiescent-node-buffer-failed "failed"
  "A regular expression to match instances where the node buffer failed to start.")

(defun quiescent-node-name-for-this-file ()
  "Compute the name of the node process buffer for this file."
  (format "*node-%s*" (buffer-name)))

(defvar quiescent-node-used-ports '()
  "A list of the ports used to spin up indium interactive processes.

TODO: derigester ports when the associated process dies.")

(defun quiescent-random-node-port ()
  "Produce a random port for node to run on which isn't already in use."
  (let (result)
    (while (member (setq result (+ 8000 (random 1000)))
                   quiescent-node-used-ports))
    result))

(defun quiescent-start-node-with-this-file ()
  "Start node in the current buffer with the debugger."
  (interactive)
  (quiescent--start-process-with-this-file "node"))

(defun quiescent-start-mocha-with-this-file ()
  "Start mocha in the current buffer with the debugger.

Requires that there exists file <project-root>/tests/.test.setup.js
which does any necessary prep for the test."
  (interactive)
  (quiescent--start-process-with-this-file
   "mocha"
   (list "--inspect" (format "%stests/.test.setup.js"
                             (projectile-project-root)))))

;;(require 'quiescent/projectile)

;; Left these here because I think that they might be useful
(defun quiescent-babel-process-name (buffer-name)
  "Produce a babel process name for BUFFER-NAME."
  (format "*babel-process-%s*" (buffer-name)))

(defvar quiescent-use-babel-with-launched-files t
  "Whether files should first be translated before being debugged.")

(defun quiescent-babel-translate-this-file ()
  "Translate this file and store it at `<file-name-sans-extension>.babel.js'.
Produces the new file name."
  (let ((process-name    (quiescent-babel-process-name (buffer-name)))
        (babel-file-name (concat (file-name-sans-extension (buffer-file-name)) ".babel.js")))
    (start-process process-name process-name
                   "babel" "--preset" "es2015,react" (buffer-file-name) "-o" babel-file-name)
    babel-file-name))

(defun quiescent-start-jest-with-this-file ()
  "Start jest in the current buffer with the debugger."
  (interactive)
  (quiescent--start-process-with-this-file
   "babel-node"
   (list "--harmony"
         "--inspect"
         (format "%snode_modules/jest/bin/jest.js"
                 (projectile-project-root))
         "--runInBand")))

(defun quiescent--start-process-with-this-file (process &optional args)
  "Start PROCESS with `--inspect' on this buffer.

Intention is for it to be used by indium mode.

Supply the (optional) list of ARGS to that process."
  (let* ((node-process-name (quiescent-node-name-for-this-file))
         (random-port       (quiescent-random-node-port))
         (file-path         (buffer-file-name))
         (node-process
          (progn
            (when (bufferp (get-buffer node-process-name))
              (with-current-buffer node-process-name (erase-buffer)))
            (apply
             #'start-process
             node-process-name
             node-process-name
             process
             (cons (format "--inspect-brk=%s" random-port)
                   (reverse (cons file-path
                                  (reverse args)))))))
         port
         path)
    (with-current-buffer node-process-name
      (let ((buffer-text (progn
                           (while (string= "" (buffer-string))
                             (sleep-for 0 100))
                           (buffer-string))))
        (when (string-match quiescent-node-buffer-failed buffer-text)
          (error "Couldn't start node process: %s" buffer-text))
        (string-match quiescent-node-url-extracting-regexp
                      buffer-text)
        (setq port (substring buffer-text (match-beginning 1) (match-end 1))
              path (substring buffer-text (match-beginning 2) (match-end 2)))))
    (indium-nodejs--connect "127.0.0.1"
                            port
                            path)))

(defun quiescent-last-message ()
  "Produce the last message logged, as the last line in `*Messages*'."
  (with-current-buffer "*Messages*"
    (goto-char (point-max))
    (beginning-of-line)
    (forward-line -1)
    (buffer-substring (point) (progn (end-of-line)
                                     (point)))))

(defun quiescent-overlay-of-fun (orig-fun &rest args)
  "Run ORIG-FUN with ARGS and create an overlay with it."
  (progn
    (apply orig-fun args)
    (sleep-for 0 100)
    (quiescent-haskell-result-overlay-at-point (quiescent-last-message))))

(advice-add #'indium-eval-last-node :around #'quiescent-overlay-of-fun)

;; To start debugging, open the following URL in Chrome:
;;     chrome-devtools://devtools/bundled/inspector.html?experiments=true&v8only=true&ws=127.0.0.1:9229/a8ef938b-045a-4098-b50e-e1a18cf415c7

(use-package indium
  :ensure t)

(defun quiescent-is-js2-mode ()
  "Produce t if this buffer is in `js2-mode'."
  (eq major-mode 'js2-mode))

(defun quiescent-indium-interaction-mode ()
  "Activate `indium-interaction-mode'."
  (when (null quiescent-starting-up)
    (indium-interaction-mode 1)))

(add-hook 'js2-mode-hook #'quiescent-indium-interaction-mode)

(use-package js2-refactor
  :ensure t)

(defun quiescent-pluse-ignoring-args (&rest args)
  "Run `xref-pulse-momentarily' ignoring ARGS."
  (xref-pulse-momentarily))

(advice-add #'js2-jump-to-definition :after #'quiescent-pluse-ignoring-args)

(defun quiescent-xref-jump-when-point-doesnt-move (jump-fun &rest args)
  "Execute JUMP-FUN with ARGS, jump with xref if it didn't move."
  (let ((start (point)))
    (ignore-errors (apply jump-fun args))
    (when (eql start (point))
      (xref-find-definitions (thing-at-point 'symbol)))))

(advice-add #'js2-jump-to-definition :around #'quiescent-xref-jump-when-point-doesnt-move)

(provide 'quiescent/javascript)
;;; javascript ends here
