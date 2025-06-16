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
  :custom (undo-tree-visualizer-diff t)
  :bind (("C-x u" . undo-tree-visualize)
         ("C-z" . undo)
         ("C-S-z" . undo-tree-redo)))

(use-package smartparens
  :diminish
  :config (sp-local-pair 'org-mode "=" "=")
  :hook
  ((org-mode prog-mode shell-mode conf-mode) . smartparens-mode))

;; Non GNU Elpa shenanigans
;; https://github.com/radian-software/straight.el/issues/1191
(use-package ws-butler
  :straight (ws-butler :type git :host github :repo "lewang/ws-butler")
  :diminish
  :hook ((prog-mode org-mode conf-mode) . ws-butler-mode))

(use-package aggressive-indent
  :config (global-aggressive-indent-mode nil)
  (add-to-list 'aggressive-indent-excluded-modes 'go-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'go-ts-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'python-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'python-ts-mode)
  :hook ((emacs-lisp-mode) . (lambda () (aggressive-indent-mode t))))


(setq reb-re-syntax 'string)
(add-hook 'reb-mode-hook
          (lambda()
            (define-key reb-mode-map "C-c C-q" 'reb-quit)))


(use-package hungry-delete
  :config
  (global-hungry-delete-mode t)
  :custom (hungry-delete-except-modes '(minibuffer-mode)))

;; Hide/show code blocks
(add-hook 'prog-mode-hook (lambda() (hs-minor-mode 1)))
(bind-key "<f8> h" 'hydra-hideshow/body)

(provide 'init-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
