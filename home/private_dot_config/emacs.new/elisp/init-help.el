;;; INIT-HELP --- Summary
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

(use-package which-key
  :diminish
  :config
  (which-key-mode t)
  (setq which-key-idle-delay 0))

(diminish 'eldoc-mode)

(use-package amx
  :config (amx-mode t))


(use-package helpful
  :custom (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :custom-face (helpful-heading ((t (:foreground "GreenYellow" :weight bold))))
  :bind (("C-h h" . #'helpful-at-point)
         ("C-h C" . #'helpful-command)
         ("C-h F" . #'helpful-function)
         ("C-h M" . #'helpful-macro)
         ([remap describe-key] . helpful-key)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-function] . helpful-callable)
         ([remap describe-symbol] . helpful-callable)
         ([remap describe-command] . helpful-command)))


(provide 'init-help)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-help.el ends here
