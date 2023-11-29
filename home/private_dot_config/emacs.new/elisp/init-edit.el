;;; INIT-EDIT --- Summary
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


(use-package undo-tree
  :diminish
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-visualizer-diff t)
  :bind (("C-z" . undo)
         ("C-S-z" . undo-tree-redo)))

(use-package smartparens
  :diminish
  :config (sp-local-pair 'org-mode "=" "=")
  :hook
  ((org-mode prog-mode shell-mode) . smartparens-mode))

(use-package ws-butler
  :diminish
  :hook ((prog-mode org-mode conf-mode) . ws-butler-mode))

(use-package aggressive-indent
  :config (global-aggressive-indent-mode t))


(setq reb-re-syntax 'string)
(add-hook 'reb-mode-hook
          (lambda()
            (define-key reb-mode-map "C-c C-q" 'reb-quit)))


(provide 'init-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
