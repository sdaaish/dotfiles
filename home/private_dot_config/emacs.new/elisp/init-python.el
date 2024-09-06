;;; INIT-PYTHON --- Summary
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


;; Python settings

;; Use tree-sitter for Python

(use-package highlight-indent-guides
  :hook ((yaml-mode yaml-ts-mode python-mode python-ts-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (python-indent-guess-indent-offset-verbose nil))

(use-package python
  :bind (:map python-mode-map
              ("C-c M-j" . run-python)
              ("C-c C-k" . python-shell-send-buffer)
              ("C-c C-c" . python-shell-send-defun)
              ("C-c C-p" . python-shell-send-defun)
              ("C-x C-e" . python-shell-send-statement))
  :custom
  (python-indent-guess-indent-offset-verbose nil)
  ;;  (python-check-command "poetry run pyright")
  (python-indent-offset 4))

;; Save for future use, read the pet man page
;; (use-package python-pytest)
;; (use-package python-black)
;; (use-package python-isort)
;; (use-package ruff-format)

(use-package pet
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

(provide 'init-python)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
