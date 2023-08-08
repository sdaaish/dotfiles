;;; init.el --- Summary

;;; Commentary:

;;; Code:

(if (>= emacs-major-version 29)
    (require 'bind-key))
;;(require 'use-package)
(require 'json)
(require 'project)
(require 'python)
;;(require 'eglot)
(require 'org)
(require 'eldoc)
(require 'tramp)

(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))
(load custom-file)

;; LISP directories
(when (not (file-exists-p "lisp"))
  (make-directory (concat user-emacs-directory "lisp") t))
(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(setq lispdir (let (lispdir)
                (expand-file-name ".config/lisp/" (getenv "HOME"))))
(when (file-exists-p lispdir)
  (add-to-list 'load-path lispdir))

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

(setq-default hl-line-mode t
              cursor-type 'bar)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(visual-line-mode 1)
(blink-cursor-mode 2)
(setq blink-cursor-blinks 2
      visible-bell t
      visible-cursor t)

;; Interactively do things
(when (not (file-exists-p ".cache"))
  (make-directory (concat user-emacs-directory ".cache") t))
(ido-mode 1)
(setq ido-everywhere t
      ido-enable-flex-matching t
      ido-use-virtual-buffers 'auto
      ido-save-directory-list-file (expand-file-name ".cache/ido.last" user-emacs-directory))

;; Completion
(fido-mode t)
(fido-vertical-mode t)
(which-function-mode t)

;; Backup and saving place in files
(save-place-mode 1)
(when (not (file-exists-p "backups"))
  (make-directory (concat user-emacs-directory "backups") t))
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backups" user-emacs-directory))))
;; (setq auto-save-file-name-transforms
;;       `((".*" . ,(expand-file-name "backups" user-emacs-directory))))
(setq delete-old-versions t
      version-control t
      vc-make-backup-files t
      backup-by-copying t
      kept-new-versions 10
      kept-old-versions 10
      auto-save-interval 40
      delete-by-moving-to-trash t
      save-silently t)

(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t
      tab-always-indent 'complete)
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq save-abbrevs 'silently
      abbrev-en-file (expand-file-name "abbrev_english_defs" "~/.config/lisp/"))
(if (file-readable-p abbrev-en-file)
    (read-abbrev-file abbrev-en-file))

;; Org mode
(setq org-directory (expand-file-name "Org/" (getenv "ONEDRIVE"))
      org-agenda-files (list org-directory)
      org-fontify-quote-and-verse-blocks t
      org-mode-hook 'abbrev-mode)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Dired
(add-hook 'dired-mode-hook
          (lambda ()
            (keymap-set dired-mode-map "'" 'dired-up-directory)
            (dired-hide-details-mode 1)))

;; Initial size of the frame
(toggle-frame-maximized)

(provide 'init)
;;; init.el ends here
