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
;; is not tested on other versions.  But since the config is with only the built-in
;; packages, this could be fairly easy to achieve.

;; With Emacs 29, the '--init-dir' can be used on the command-line, which makes it
;; possible to use multiple different configs on a system, without any third-party
;; packages (Chemacs2 and others).

;; WIP

;;; Code:

;; Startup optimization
(setq gc-cons-threshold (* 64 1024 1024))
(defun gc/set-after-start ()
  "Set a sane value after starting Emacs."
  (setq gc-cons-threshold (* 16 1024 1024)))
(setq after-init-hook 'gc/set-after-start)

(if (>= emacs-major-version 29)
    (require 'bind-key))
;;(require 'json)
;;(require 'project)
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

;; Modus themes settings
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
      modus-themes-subtle-line-numbers t
      modus-themes-headings
      '((0 . (background overline 1.5))
	      (1 . (background overline 1.3))
        (2 . (overline rainbow 1.2))
        (3 . (overline 1.1))
        (t . (monochrome))))

(load-theme 'modus-operandi t t)

(enable-theme 'modus-vivendi)
(define-key global-map (kbd "S-<f5>") #'modus-themes-toggle)
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
      uniquify-buffer-name-style 'forward
      large-file-warning-threshold nil
      ad-redefinition-action 'accept)

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
;;(setq auto-save-file-name-transforms
;;      `((".*" ,(expand-file-name "backups" user-emacs-directory) t)))
(save-place-mode 1)
(auto-save-mode 1)

(setq save-place-file (expand-file-name ".cache/saveplace" user-emacs-directory))
(setq recentf-save-file (expand-file-name ".cache/recentfiles" user-emacs-directory))

(setq delete-old-versions t
      version-control t
      vc-make-backup-files t
      vc-follow-symlinks t
      backup-by-copying t
      kept-new-versions 10
      kept-old-versions 10
      auto-save-interval 40
      auto-save-no-message t
      delete-by-moving-to-trash t
      save-silently t)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq inhibit-startup-message t
      tab-always-indent 'complete
      native-comp-async-report-warnings-errors nil)
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

(setq initial-scratch-message ";; yo!\n\n")
(setq initial-major-mode 'emacs-lisp-mode)

;; Setup an emoji font depending on OS
(cond ((member "Segoe UI Emoji" (font-family-list))
       (set-fontset-font t 'symbol (font-spec :family "Segoe UI Emoji" :fontified t) nil 'prepend)
       (set-fontset-font t 'unicode (font-spec :family "Segoe UI Emoji" :fontified t) nil 'prepend))
      ((member "Noto Color Emoji" (font-family-list))
       (set-fontset-font t 'symbol (font-spec :family "Noto color emoji" :fontified t) nil 'prepend)
       (set-fontset-font t 'unicode (font-spec :family "Noto color emoji" :fontified t) nil 'prepend)))

;; Programming options
(defun my/display-line-numbers()
  "Display line numbers in the buffer."
  (interactive)
  (setq display-line-numbers-type 'relative)
  (display-line-numbers-mode 1))

(add-hook 'prog-mode-hook (electric-pair-mode t))
(add-hook 'prog-mode-hook 'my/display-line-numbers)

;; Python settings
(setq python-indent-guess-indent-offset-verbose nil)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'python-ts-mode-hook
          (lambda ()
	          (setq-local compile-command
		                    (concat "mypy "
			                          (if buffer-file-name
			                              (shell-quote-argument
			                               (file-name-sans-extension buffer-file-name)))))))
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c S-C-c") 'compile)
  (define-key python-mode-map (kbd "C-c S-C-r") 'recompile))

;; Flymake
(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "C-c C-g") 'flymake-show-buffer-diagnostics)
  (add-to-list 'display-buffer-alist
               '("^\\*Flymake diagnostics"
                 (display-buffer-reuse-window display-buffer-pop-up-window)
                 (window-height . 8))))

;; Eglot
(setq eglot-ignored-server-capabilities '(:documentHighlightProvider)
      eglot-report-progress nil)
(setq-default eglot-workspace-configuration
              '((:gopls .
                        ((staticcheck . t)
                         (matcher . "CaseSensitive")))))
(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))

;; Tree-sitter language definitions
(when (file-directory-p "~/.config/tree-sitter/")
  (setq treesit-extra-load-path (list (expand-file-name "~/.config/tree-sitter/"))))

;; Go lang
(add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-hook 'go-ts-mode-hook 'eglot-ensure 10)
(add-hook 'go-ts-mode-hook 'flymake-mode 8)
(add-hook 'go-ts-mode-hook 'flymake-show-buffer-diagnostics 9)
(add-hook 'go-ts-mode-hook #'eglot-format-buffer-on-save)

(defun go-compile()
  "Compile the current buffer"
  (interactive)
  (compile (concat "go run " (file-name-nondirectory (buffer-file-name)))))

(defun go-build()
  "Build the current buffer"
  (interactive)
  (compile (concat "go build " (file-name-nondirectory (buffer-file-name)))))

(with-eval-after-load 'go-ts-mode
  (define-key go-ts-mode-map (kbd "C-c C-c") 'go-compile)
  (define-key go-ts-mode-map (kbd "C-c C-r") 'recompile)
  (define-key go-ts-mode-map (kbd "C-c C-b") 'go-build)
  (define-key go-ts-mode-map (kbd "C-c C-i") 'eglot-code-action-organize-imports))

;; Start the server
(server-start)

(provide 'init)
;;; init.el ends here
