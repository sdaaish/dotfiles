;;; INIT-ESHELL --- Summary
;;
;; Author: Stig Dahl
;; Created: 2023-12-05
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


(when (eq system-type 'gnu/linux)
  (setenv "PAGER" "cat"))

(use-package eshell
  :straight (:type built-in)
  :custom
  (eshell-prefer-lisp-functions t)
  (eshell-prefer-lisp-variables t)
  (eshell-history-size 10000)
  (eshell-buffer-maximum-lines 10000)
  (eshell-hist-ignoredups t)
  (eshell-scroll-to-bottom-on-input t)
  (eshell-aliases-file (my/setup-dirs ".config/eshell/alias" (getenv "HOME")) (getenv "HOME"))
  (password-cache t)
  (password-cache-expiry 3600)
  :bind ("C-c RET" . eshell))

;; Trying out this
(use-package eshell-prompt-extras
  :custom (eshell-highlight-prompt nil)
  (eshell-prompt-function 'epe-theme-dakrone)
  (epe-git-dirty-char "💩"))

;; Not eshell, but put it here anyway
;;(when (eq system-type 'windows-nt)
;;  (setq shell-file-name (default-value 'shell-file-name)))

(provide 'init-eshell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eshell.el ends here
