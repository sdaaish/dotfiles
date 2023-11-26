;;; INIT --- Summary
;;
;; Author: Stig Dahl
;; Created: 2023-11-25
;;

;;
;;; Commentary:
;;

;; This is a completely new Emacs config, to make it the one to rule them all....

;;
;;; Change log:
;;

;;
;;; Code:
;;

;; Startup optimization
(setq gc-cons-threshold (* 50 1000 1000))
(defun gc/set-after-start ()
  "Set a sane value after starting Emacs."
  (setq gc-cons-threshold (* 2 1000 1000)))
(setq after-init-hook 'gc/set-after-start)

(if (>= emacs-major-version 29)
    (require 'bind-key))

;; Use straight package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package straight
  :custom 
(straight-recipes-emacsmirror-use-mirror t)
(straight-use-package-by-default t)
(straight-host-usernames
                        '((github . "sdaaish")
                          (gitlab . "sdaaish"))))

(straight-use-package 'org)

(use-package bind-key)
(use-package diminish)

;; Modules
(when (not (file-exists-p "modules"))
  (make-directory (concat user-emacs-directory "modules") t))
(add-to-list 'load-path (concat user-emacs-directory "modules"))

;; LISP directories
(when (not (file-exists-p "elisp"))
  (make-directory (concat user-emacs-directory "elisp") t))
(add-to-list 'load-path (concat user-emacs-directory "elisp"))

;; Shared LISP directory
(setq lispdir (let (lispdir)
                (expand-file-name ".config/lisp/" (getenv "HOME"))))
(when (file-exists-p lispdir)
  (add-to-list 'load-path lispdir))

(display-time-mode t)
(setq display-time-24hr-format t)

(use-package company
  :config (global-company-mode 1)
  :diminish)

;; Key bindings
(bind-key "<f8> i" (lambda()
                     (interactive)
                     (find-file user-init-file)))


;; Emacs customize in separate file
(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


;; Modus themes settings
;;; For packaged versions which must use `require':
;;(straight-use-package 'modus-themes :source '(melpa))

(use-package modus-themes
 :custom (modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-org-blocks 'tinted-background
      modus-themes-variable-pitch-ui t
      modus-themes-hl-line '(accented intense)
      modus-themes-paren-match '(bold intense)
      modus-themes-mixed-fonts t
      modus-themes-prompts '(regular bold)
      modus-themes-to-toggle '(modus-operandi modus-vivendi)
      modus-themes-completions
      '((matches . (extrabold underline))
        (selection . (semibold italic)))
      modus-themes-mode-line '(moody borderless accented)
      modus-themes-subtle-line-numbers t
      modus-themes-headings
      '((0 . (background overline 1.5))
              (1 . (background overline 1.3))
        (2 . (overline rainbow 1.2))
        (3 . (overline 1.1))
        (t . (monochrome))))
 :config
 (load-theme 'modus-vivendi t t)
 (load-theme 'modus-operandi t nil)
 (enable-theme 'modus-operandi)
 :bind ("S-<f5>" . 'modus-themes-toggle))

(setq mode-line-compact t)
(size-indication-mode 1)

;; Visual preferences
(require 'uniquify)
(global-hl-line-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(visual-line-mode 1)
(blink-cursor-mode 2)
(column-number-mode t)
(setq-default cursor-type 'bar
              cursor-in-non-selected-windows nil)
(setq blink-cursor-blinks 2
      visible-bell t
      visible-cursor t
      ring-bell-function 'ignore
      use-dialog-box nil
      uniquify-buffer-name-style 'forward)


(when (not (file-exists-p ".cache"))
  (make-directory (concat user-emacs-directory ".cache") t))

;; Backup and saving place in files
(when (not (file-exists-p "backups"))
  (make-directory (concat user-emacs-directory "backups") t))
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backups" user-emacs-directory))))
;;(setq auto-save-file-name-transforms
;;      `((".*" ,(expand-file-name "backups" user-emacs-directory) t)))
(save-place-mode 1)
(auto-save-mode 1)
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

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

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

(require 'init-magit)
(require 'init-chezmoi)

;; Fix things below here


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
  "Set the frame size when Emacs starts up.  Depending on display-size,
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

(setq initial-scratch-message ";; scratch!\n\n")
(setq initial-major-mode 'emacs-lisp-mode)

;; Setup an emoji font depending on OS
(cond ((member "Segoe UI Emoji" (font-family-list))
       (set-fontset-font t 'symbol (font-spec :family "Segoe UI Emoji" :fontified t) nil 'prepend)
       (set-fontset-font t 'unicode (font-spec :family "Segoe UI Emoji" :fontified t) nil 'prepend))
      ((member "Noto Color Emoji" (font-family-list))
       (set-fontset-font t 'symbol (font-spec :family "Noto color emoji" :fontified t) nil 'prepend)
       (set-fontset-font t 'unicode (font-spec :family "Noto color emoji" :fontified t) nil 'prepend)))


;; Python settings
(setq python-indent-guess-indent-offset-verbose nil)
(add-hook 'python-mode-hook 'eglot-ensure)


(provide 'init.el)
;;; init.el ends here
