;;; init.el --- Summary

;;; Commentary:

;;; Code:

(require 'bind-key)
(require 'use-package)
(require 'json)
(require 'project)
(require 'python)
(require 'eglot)
(require 'org)
(require 'eldoc)
(require 'tramp)
;;(require 'isearchb)

(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))
(load custom-file)

(display-time-mode t)
(setq display-time-24hr-format t)

(require 'modus-themes)
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-org-blocks 'tinted-background
      modus-themes-variable-pitch-ui t
      modus-themes-hl-line '(accented intense)
      modus-themes-paren-match '(bold intense)
      modus-themes-mixed-fonts t
      modus-themes-prompts '(regular bold)
      modus-themes-completions
      '((matches . (extrabold underline))
        (selection . (semibold italic)))
      modus-themes-mode-line '(moody borderless accented)
      modus-themes-headings
      '((0 . (background overline 1.5))
	(1 . (background overline 1.3))
              (2 . (overline rainbow 1.2))
              (3 . (overline 1.1))
              (t . (monochrome))))

;; Modus themes
(load-theme 'modus-operandi t t)
(load-theme 'modus-vivendi t t)
(enable-theme 'modus-vivendi)
(define-key global-map (kbd "S-<f5>") #'modus-themes-toggle)

(setq-default hl-line-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(visual-line-mode 1)
(save-place-mode 1)
(setq backup-directory-alist
      '((".*" . (expand-file-name "backups" user-emacs-directory))))

(setq inhibit-startup-message t
      tab-always-indent 'complete)
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(defalias 'yes-or-no-p 'y-or-n-p)

;; (define-key global-map [(control ?z)] 'isearchb-activate)
;; (setq isearchb-idle-timeout 2)
;; (setq isearch-repeat-on-direction-change t)

;; Org mode
(setq org-directory (expand-file-name "Org/" (getenv "ONEDRIVE"))
      org-agenda-files (list org-directory)
      org-fontify-quote-and-verse-blocks t)

(provide 'init)
;;; init.el ends here
