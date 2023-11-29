;;; INIT-COMMON --- Summary
;;
;; Author:  Stig Dahl
;; Created: tisdag november 2023-11-28
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Common settings for Emacs.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(setq message-log-max (* 256 1024))

(setq inhibit-splash-screen t
      inhibit-startup-screen t
      initial-major-mode 'org-mode
      initial-scratch-message ";; scratch!\n\n")

(setq-default major-mode 'text-mode)
(display-line-numbers-mode -1)
(column-number-mode t)
(transient-mark-mode t)
(show-paren-mode t)
(customize-set-variable 'show-paren-style 'mixed)
(setq-default line-spacing 1)
(setq-default show-trailing-whitespace nil)
(setq-default indicate-empty-lines t)
(customize-set-variable 'apropos-do-all t)
(global-subword-mode t)
(diminish 'subword-mode)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default tab-always-indent 'complete)      ;;Use tabs as indents, 2ch width

(setq mode-require-final-newline t
      next-line-add-newlines nil
      require-final-newline t)

(recentf-mode 1)
(setq recentf-max-menu-items 25)

(customize-set-variable 'global-auto-revert-mode t)
(customize-set-variable 'global-auto-revert-non-file-buffers t)

(customize-set-variable 'bookmark-save-flag 1)
(customize-set-variable 'bookmark-version-control t)

(require 'whitespace)

(setq-default visual-line-mode t)
(setq-default case-fold-search t)

(customize-set-variable 'read-buffer-completion-ignore-case t)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)

(customize-set-variable 'use-dialog-box nil)

(provide 'init-common)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-common.el ends here
