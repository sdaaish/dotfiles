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

(defun shell-mode-company-init ()
  "Company for terminal shell."
  (setq-local company-backends '((company-capf
                                  company-keywords
                                  company-files
                                  company-dabbrev-code))))

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
  :bind ("C-c RET" . eshell)
  :hook (eshell-mode . shell-mode-company-init))

;; Trying out this
(use-package eshell-prompt-extras
  :custom (eshell-highlight-prompt nil)
  (eshell-prompt-function 'epe-theme-dakrone)
  (epe-git-dirty-char "🔺"))

;; Not eshell, but put it here anyway. This is for the interactive shell, not the shell-command.
;; Changing the `shell-file-name`, i.e. `shell-command` did break things.
(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "pwsh.exe"))

(use-package shell
  :straight (:type built-in)
  :hook (shell-mode . shell-mode-company-init))

;; EAT is a emulating terminal for Emacs. (Emulate A Terminal).
;; https://codeberg.org/akib/emacs-eat#headline-3
(unless (eq system-type 'windows-nt)
  (use-package eat
    ;; For `eat-eshell-mode'.
    :hook (eshell-load . eat-eshell-mode)))

;; For `eat-eshell-visual-command-mode'.
;;(add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

;; Install VTERM for nix systems
;; Requires cmake, libtool-bin, libvterm
(unless (eq system-type 'windows-nt)
  (use-package vterm
    :hook (vterm-mode-hook .
                           (lambda ()
                             (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
                             (buffer-face-mode t)))))

(provide 'init-eshell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eshell.el ends here
