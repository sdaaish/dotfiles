;;; INIT-CODE --- Summary
;;
;; Author: Stig Dahl
;; Created: 2023-12-09
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

(use-package editorconfig
  :diminish)

(use-package zig-mode)

;; devdocs.io viewer
(use-package devdocs
  :bind ("C-," . devdocs-lookup)
  ("C-h D" . hydra-devdocs/body)
  :hook
  (emacs-lisp-mode . (lambda () (setq-local devdocs-current-docs '("elisp"))))
  (python-mode . (lambda () (setq-local devdocs-current-docs '("python~3.12"))))
  (zig-mode . (lambda () (setq-local devdocs-current-docs '("zig"))))
  (go-mode . (lambda () (setq-local devdocs-current-docs '("go")))))


;; Remember to copy the libraries to the correct folder and rename them.
(use-package tree-sitter)
(use-package tree-sitter-langs)

(provide 'init-code)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-code.el ends here
