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
  :bind (:map python-ts-mode-map
              ("C-c S-C-c" . 'compile)
              ("C-c S-C-r" . 'recompile))
  :custom
  (python-indent-guess-indent-offset-verbose nil)
  (python-check-command "ruff check")
  (python-indent-offset 4)
  :hook ((python-mode python-ts-mode) .
         (lambda ()
           (setq-local compile-command
		                   (concat "mypy "
			                         (if buffer-file-name
			                             (shell-quote-argument
			                              (file-name-sans-extension buffer-file-name))))))))

;; Save for future use, read the pet man page
;;(use-package py-autopep8)
;;(use-package yapfify)
(use-package blacken)

(use-package python-pytest)

(use-package python-isort
  :hook (python-ts-mode 'python-isort-on-save-mode))

(use-package ruff-format
  :hook (python-ts-mode . 'ruff-format-on-save-mode))

(use-package pet
  :hook
  (python-base-mode . (lambda () (pet-mode -10))))
;;  ((python-mode python-ts-mode) . (lambda ()
;;                                    (setq-local python-shell-interpreter (pet-executable-find "python")
;;                                                python-shell-virtualenv-root (pet-virtualenv-root)))))

(provide 'init-python)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
