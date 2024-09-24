;;; INIT-SEARCH --- Summary
;;
;; Author: Stig Dahl
;; Created: 2023-11-29
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(use-package ag
  :commands counsel-ag)

(use-package rg
  :config (rg-enable-default-bindings))

(use-package grep
  :straight (:type built-in)
  :config
  (when (executable-find "rg")
    (setq grep-program "rg")
    (grep-apply-setting
     'grep-find-command
     '("rg -n -H --color always --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 42))))

(use-package macrostep
  :bind ("C-c e" . macrostep-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless-flex basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(defun my/match-components-literally ()
  "Components match literally for the rest of the session."
  (interactive)
  (setq-local orderless-matching-styles '(orderless-literal)
              orderless-style-dispatchers nil))

(define-key minibuffer-local-completion-map (kbd "C-l")
            #'my/match-components-literally)

(use-package elgrep)

(provide 'init-search)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-search.el ends here
