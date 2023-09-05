;;; init.el --- Default configuration of Emacs  -*- lexical-binding: t; -*-

;;; Commentary:

;; This is a default or minimal configuration of Emacs to use as the sane default
;; on a new system or when things go bad.
;; This config don't use any external packages, not even Gnu Elpa or NonGnu Elpa,
;; only the already built-in.

;; This to make it fast and reliable to start, so it is useable as a backup or
;; spare configuration of the editor.

;; It is also a practical way to learn the inner of Emacs and of it's historic
;; commands and settings.

;; This config will most likely only work out-of-the-box with Emacs 29 at the moment,
;; is not tested on other versions. But since the config is with only the built-in
;; packages, this could be fairly easy to achieve.

;; With Emacs 29, the '--init-dir' can be used on the command-line, which makes it
;; possible to use multiple different configs on a system, without any third-party
;; packages (Chemacs2 and others).

;; WIP

;;; Code:

;; Startup optimization
(setq gc-cons-threshold (* 50 1000 1000))
(defun gc/set-after-start ()
  "Set a sane value after starting Emacs."
  (setq gc-cons-threshold (* 2 1000 1000)))
(setq after-init-hook 'gc/set-after-start)

(if (>= emacs-major-version 29)
    (require 'bind-key))
;;(require 'use-package)
;;(require 'json)
;;(require 'project)
;;(require 'python)
;;(require 'eglot)
;;(require 'org)
;;(require 'eldoc)
;;(require 'tramp)

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
(column-number-mode t)
(custom-set-variables
 '(blink-cursor-blinks 2)
 '(visible-bell t)
 '(visible-cursor t)
 '(ring-bell-function 'ignore)
 '(use-dialog-box nil))

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
(when (not (file-exists-p "backups"))
  (make-directory (concat user-emacs-directory "backups") t))
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backups" user-emacs-directory))))
;; (setq auto-save-file-name-transforms
;;       `((".*" . ,(expand-file-name "backups" user-emacs-directory))))
(save-place-mode 1)
(setq save-place-file (expand-file-name ".cache/saveplace" user-emacs-directory))
(setq recentf-save-file (expand-file-name ".cache/recentfiles" user-emacs-directory))

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
(global-set-key (kbd "C-z") 'zap-up-to-char)
(global-set-key (kbd "C-<kp-add>") 'text-scale-increase)
(global-set-key (kbd "C-<kp-subtract>") 'text-scale-decrease)
(setq text-scale-mode-step 1.05)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Abbreviations
(setq save-abbrevs 'silently
      abbrev-en-file (expand-file-name "abbrev_english_defs" "~/.config/lisp/"))
(if (file-readable-p abbrev-en-file)
    (read-abbrev-file abbrev-en-file))
(setq-default abbrev-mode t)

;; Org mode
(setq org-directory (expand-file-name "Org/" (getenv "ONEDRIVE"))
      org-agenda-files (list org-directory)
      org-fontify-quote-and-verse-blocks t
      org-mode-hook (lambda () (abbrev-mode nil)))
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Dired
(add-hook 'dired-mode-hook
          (lambda ()
            (keymap-set dired-mode-map "'" 'dired-up-directory)
            (dired-hide-details-mode 1)))
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-kill-when-opening-new-dired-buffer t)

;; Info mode
(with-eval-after-load 'info
  (keymap-set Info-mode-map "'" 'Info-up))

;; Initial size of the frame, if wide screen, center the frame.
;; Else, maximize it (not full screen).
(defun my/set-frame-size (p1 p2 x y)
  "Sets the frame size when Emacs starts up. Depending on display-size,
  the frame try to adjust to that."
  (set-frame-position nil p1 p2)
  (set-frame-width nil x)
  (set-frame-height nil y))

(cond ((>= (nth 3 (assq 'geometry (frame-monitor-attributes))) 5000)
       (my/set-frame-size 1000 20 200 40))
      ((>= (nth 3 (assq 'geometry (frame-monitor-attributes))) 3000)
       (my/set-frame-size 600 20 200 50))
      (t (toggle-frame-maximized nil)))

(add-hook 'window-size-change-functions
          #'frame-hide-title-bar-when-maximized)

;; Remember windows
(winner-mode 1)

;; Move more easily
(windmove-mode 1)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
;; Make windmove work in Org mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(setq initial-scratch-message ";; yo!\n\n")
(setq initial-major-mode 'emacs-lisp-mode)

;; Setup an emoji font depending on OS
(cond ((member "Segoe UI Emoji" (font-family-list))
       (set-fontset-font t 'symbol (font-spec :family "Segoe UI Emoji" :fontified t) nil 'prepend)
       (set-fontset-font t 'unicode (font-spec :family "Segoe UI Emoji" :fontified t) nil 'prepend))
      ((member "Noto Color Emoji" (font-family-list))
       (set-fontset-font t 'symbol (font-spec :family "Noto color emoji" :fontified t) nil 'prepend)
       (set-fontset-font t 'unicode (font-spec :family "Noto color emoji" :fontified t) nil 'prepend)))

(provide 'init)
;;; init.el ends here
