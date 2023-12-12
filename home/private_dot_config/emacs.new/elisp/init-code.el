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
  :diminish
  :hook (prog-mode . editorconfig-mode))


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

(provide 'init-code)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-code.el ends here
