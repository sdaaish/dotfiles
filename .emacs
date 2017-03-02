;;
;; My emacs settings config
;; SDAA
;;
(message "Reading configuration file...")

;;Package settings
;; Marmalade cert issue
;; From https://github.com/nicferrier/elmarmalade/issues/55
;; Certificate pinning https://blogs.fsfe.org/jens.lechtenboerger/2014/03/23/certificate-pinning-for-gnu-emacs/
;;(if (fboundp 'gnutls-available-p)
;;    (fmakunbound 'gnutls-available-p))
;;(setq tls-program '("gnutls-cli --tofu -p %p %h")
;;      imap-ssl-program '("gnutls-cli --tofu -p %p %s")
;;      smtpmail-stream-type 'starttls
;;      starttls-extra-arguments '("--tofu")
;;      )

(require 'package)
(setq package-check-signature nil)	;;Fix for marmalade repo
(setq package-archives '(("org"		. "http://orgmode.org/elpa/")
			 ("gnu"		. "https://elpa.gnu.org/packages/")
                         ("melpa-stable"       . "https://stable.melpa.org/packages")
                         ("melpa"       . "https://melpa.org/packages")))
(package-initialize)

;; Loads settings from this directory
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Use this file for builtin Customize-function
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Startup settings
(setq inhibit-splash-screen t
      inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;;No beep
(setq visible-bell t)

;; Customization
(tool-bar-mode -1)
(menu-bar-mode -1)
(line-number-mode t)
(column-number-mode t)
(transient-mark-mode t)
(show-paren-mode 1)
(setq-default line-spacing 1)
(setq tab-width 2
      tab-always-indent t)	;;Use tabs as indents, 2ch width


;; Newline settings
(setq mode-require-final-newline t)
(setq next-line-add-newlines nil)
(setq require-final-newline t)

;; Press y or n for yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; language
;;(set-input-mode t nil 'iso)
;;(standard-display-8bit 160 255)

;;Themes
(load-theme 'tango-dark)

;; Tidy settings
(autoload 'tidy-buffer "tidy" "Run Tidy HTML parser on current buffer" t)
(autoload 'tidy-parse-config-file "tidy" "Parse the `tidy-config-file'" t)
(autoload 'tidy-save-settings "tidy" "Save settings to `tidy-config-file'" t)
(autoload 'tidy-build-menu  "tidy" "Install an options menu for HTML Tidy." t)
(defun my-html-mode-hook () "Customize my html-mode."
  (tidy-build-menu html-mode-map)
  (local-set-key [(control c) (control c)] 'tidy-buffer)
  (setq sgml-validate-command "tidy"))
(add-hook 'html-mode-hook 'my-html-mode-hook)


(define-key global-map (kbd "RET") 'newline-and-indent)
   (add-hook 'f90-mode-hook (lambda ()
			      (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))
;; Windows stuff
;;(global-set-key "\C-a" 'mark-whole-buffer)
;;(global-set-key "\C-f" 'isearch-forward)
;;(global-set-key "\C-o" 'find-file)
;;(global-set-key "\C-s" 'save-buffer)
;;(global-set-key "\C-w" 'kill-this-buffer)

;; From https://github.com/magnars/.emacs.d
;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
		 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

