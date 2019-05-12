;;; quiescent/code-explorer --- A code exploring interface  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'hydra)
(require 'quiescent/avy)

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
  ("i" quiescent-helm-semantic-or-imenu nil)
  ("q" nil                              nil)
  ("g" project-find-file                nil))
(key-chord-define-global "qc" 'hydra-explore-code/body)

(defun quiescent-helm-semantic-or-imenu ()
  "Do `helm-semantic-or-imenu' with arg as 0."
  (interactive)
  (call-interactively #'helm-semantic-or-imenu))

(provide 'quiescent/code-explorer)
