;;; INIT-IVY --- Summary
;;
;; Author:  Stig Dahl
;; Created: 2023-11-27
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

  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (enable-recursive-minibuffers t)

  :config
  (ivy-mode 1)
  (add-to-list 'ivy-highlight-functions-alist '(orderless-ivy-re-builder . orderless-ivy-highlight))
  (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))

  :bind
  ("C-x C-f" . counsel-find-file)
  ("C-c C-S-F" . counsel-recentf)
  ("C-c C-S-R" . ivy-resume)
  ("C-s" . swiper)
  ("C-r" . swiper)
  ("C-c j" . hydra-ivy/body))

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

;; For use with counsel-search
(use-package request)

;; Use posframe to create a frame for Ivy
(use-package ivy-posframe
  :diminish
  :config (ivy-posframe-mode 1)
  :custom (ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
  (ivy-posframe-height-alist '((swiper . 15)
                               (t . 25)))
  (ivy-posframe-parameters '((left-fringe . 5)
                             (right-fringe . 5))))

(provide 'init-ivy)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ivy.el ends here
