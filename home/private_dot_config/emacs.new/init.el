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

(message "*** Reading from %s ***" (buffer-name))

;; Debug startup
(setq debug-on-error t)
(setq debug-on-quit t)

;; Startup optimization
(setq gc-cons-threshold (* 500 1024 1024))
(setq garbage-collection-messages nil)

(defun gc/set-after-start ()
  "Set a sane value after starting Emacs."
  (setq gc-cons-threshold (* 5 1024 1024)))
(setq after-init-hook 'gc/set-after-start)

;; From emacs-from-scratch https://github.com/daviwil/emacs-from-scratch/blob/master/init.el
(defun efs/display-startup-time ()
  "Prints the startup time for Emacs."
  (message "*** Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(defvar start-time (float-time (current-time)))

(defun my/format-time (time)
  "Displays formatted TIME."
  (format-time-string "%Y-%m-%d %H:%M:%S" time))

(defun my/startup-timer ()
  "Measures time differences."
  (format-time-string "%M:%S.%3N" (- (float-time (current-time)) start-time)))

(message "*** Started emacs @ %s" (my/format-time start-time))
(message "*** Reading configuration from init.el...")

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

;; Keep Emacs directory tidy
(use-package no-littering
  :config (no-littering-theme-backups))

(use-package bind-key)
(use-package diminish
  :config
  (diminish 'eldoc-mode))

;; Emacs LISP directory for configuration
(when (not (file-exists-p "elisp"))
  (make-directory (concat user-emacs-directory "elisp") t))
(add-to-list 'load-path (concat user-emacs-directory "elisp"))

;; Shared Emacs LISP directory
(setq lispdir (let (lispdir)
                (expand-file-name ".config/lisp/" (getenv "HOME"))))
(when (file-exists-p lispdir)
  (add-to-list 'load-path lispdir))

;; Load org early when using straight
(straight-use-package 'org)

(require 'init-common)
(require 'init-settings)
(require 'init-org)

(display-time-mode t)
(setq display-time-24hr-format t
      display-time-default-load-average nil)

(use-package company
  :config (global-company-mode 1)
  :diminish)

;; Key bindings

;; Emacs customize in separate file
(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))


;; Modus themes settings
;;; For packaged versions which must use `require':
;;(straight-use-package 'modus-themes :source '(melpa))

(use-package modus-themes
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-org-blocks 'tinted-background)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-hl-line '(accented intense))
  (modus-themes-paren-match '(bold intense))
  (modus-themes-mixed-fonts t)
  (modus-themes-prompts '(regular bold))
  (modus-themes-to-toggle '(modus-operandi modus-vivendi))
  (modus-themes-completions   '(((matches . (extrabold underline)))
                                ((selection . (semibold italic)))))
  (modus-themes-mode-line '(moody borderless accented))
  (modus-themes-subtle-line-numbers t)
  (modus-themes--disable-themes t)
  (modus-themes-headings
   '((0 . (background overline 1.5))
     (1 . (background overline 1.3))
     (2 . (overline rainbow 1.2))
     (3 . (overline 1.1))
     (t . (monochrome))))
  (modus-themes-common-palette-overrides
   '((date-common cyan)
     (date-deadline red-warmer)
     (date-event green)
     (date-holiday blue)
     (date-now yellow-warmer)
     (date-scheduled blue-warmer)
     (date-weekday cyan-warmer)
     (date-weekend blue-faint)))

  :config
  (load-theme 'modus-vivendi-tinted t nil)
  (load-theme 'modus-operandi-tinted t nil)
  (load-theme 'modus-vivendi t nil)
  (load-theme 'modus-operandi t t)
  (enable-theme 'modus-vivendi))

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
;; (when (not (file-exists-p "backups"))
;;   (make-directory (concat user-emacs-directory "backups") t))
;; (setq backup-directory-alist
;;       `((".*" . ,(expand-file-name "backups" user-emacs-directory))))

(save-place-mode 1)
(auto-save-mode 1)
(setq save-place-file (expand-file-name ".cache/saveplace" user-emacs-directory))
(setq recentf-save-file (expand-file-name ".cache/recentfiles" user-emacs-directory))

(setq delete-old-versions t
      version-control t
      vc-make-backup-files t
      backup-by-copying nil
      backup-by-copying-when-linked t
      backup-by-copying-when-mismatch t
      kept-new-versions 10
      kept-old-versions 10
      auto-save-interval 40
      delete-by-moving-to-trash t
      save-silently t)

(setq savehist-file (expand-file-name ".savehist" no-littering-var-directory))
(savehist-mode 1)
(setq
 history-length 50
 history-delete-duplicates t
 savehist-save-minibuffer-history t
 savehist-additional-variables
 '(kill-ring
   search-ring
   regexp-search-ring))

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

;; From https://stackoverflow.com/questions/17557186/turn-off-abbrev-mode-in-emacs-minibuffer
;; Disable abbrev in minibuffer.
(defun conditionally-disable-abbrev ()
  ""
  (abbrev-mode -1))

(add-hook 'minibuffer-setup-hook 'conditionally-disable-abbrev)
(add-hook 'minibuffer-exit-hook (lambda () (abbrev-mode 1)))

(diminish 'org-indent-mode)
(diminish 'abbrev-mode)
(diminish 'visual-line-mode)


(require 'init-utils)
(require 'init-dired)
(require 'init-edit)
(require 'init-search)
(require 'init-magit)
(require 'init-ivy)
(require 'init-hydra)
(require 'init-snippets)
(require 'init-display)
(require 'init-help)
(require 'init-eglot)
(require 'init-powershell)
(require 'init-golang)
(require 'init-denote)
(require 'init-fonts)
(require 'init-project)
(require 'init-python)
(require 'init-babel)
(require 'init-eshell)
(require 'init-formatting)
(require 'init-code)
(require 'init-chezmoi)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fix things below here


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

;; Start Emacs as server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Turn of debug
(setq debug-on-error nil)
(setq debug-on-quit nil)

;;; Measure the startup time
(message "*** Finished emacs @ %s in %s" (my/format-time (current-time)) (my/startup-timer))
(message "*** This is the last line of the config. Startup time=%3.5s ***" (emacs-init-time))

(provide 'init)
;;; init.el ends here
