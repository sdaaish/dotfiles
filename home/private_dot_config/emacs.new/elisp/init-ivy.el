;;; INIT-IVY --- Summary
;;
;; Author:  Stig Dahl
;; Created: 27 November 2023-11-27
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

(use-package ivy
  :diminish
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        enable-recursive-minibuffers t)
  :bind
  ("C-x C-f" . counsel-find-file)
  ("C-c C-S-F" . counsel-recentf)
  ("C-c C-S-R" . ivy-resume)
  ("C-s" . swiper)
  ("C-r" . swiper)
  ("C-c g" . counsel-git)
  ("C-c j" . counsel-git-grep)
  ("C-c k" . counsel-ag)
  ("C-c r" . counsel-rg)
  ("C-c G" . counsel-google))


(use-package swiper
  :config
  (setq ivy-use-selectable-prompt t
        swiper-action-recenter t
        swiper-include-line-number-in-search t
        swiper-goto-start-of-match t
        swiper-stay-on-quit nil)
  (set-face-background 'swiper-line-face "Light Slate Grey"))

(use-package counsel
  :diminish
  :config
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable)
  (counsel-mode 1)
  :bind
  ("M-x" . counsel-M-x)
  ("C-h f" . counsel-describe-function)
  ("C-h v" . counsel-describe-variable))

(use-package ivy-rich
  :init  (ivy-rich-mode 1)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (customize-set-variable 'ivy-rich-path-style 'abbrev))

(provide 'init-ivy)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ivy.el ends here
