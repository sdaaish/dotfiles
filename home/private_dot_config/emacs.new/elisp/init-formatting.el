;;; INIT-FORMATTING --- Summary
;;
;; Author: Stig Dahl
;; Created: 2023-12-06
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

(use-package csv-mode)
(use-package json-mode)
(use-package toml-mode)
(use-package yaml-mode)
(use-package markdown-mode)
(use-package ssh-config-mode)
(use-package ini-mode)
(use-package git-modes
  :config
  (add-to-list 'auto-mode-alist (cons "/.dockerignore\\'" 'gitignore-mode)))

(provide 'init-formatting)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-formatting.el ends here
