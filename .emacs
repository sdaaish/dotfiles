(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(next-line-add-newlines nil)
 '(require-final-newline t)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(transient-mark-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(transient-mark-mode t)
(show-paren-mode 1)
(setq inhibit-splash-screen t)
(setq-default line-spacing 1)       
;; language
;;(set-input-mode t nil 'iso)
;;(standard-display-8bit 160 255)

(autoload 'tidy-buffer "tidy" "Run Tidy HTML parser on current buffer" t)
(autoload 'tidy-parse-config-file "tidy" "Parse the `tidy-config-file'" t)
(autoload 'tidy-save-settings "tidy" "Save settings to `tidy-config-file'" t)
(autoload 'tidy-build-menu  "tidy" "Install an options menu for HTML Tidy." t)
(defun my-html-mode-hook () "Customize my html-mode."
  (tidy-build-menu html-mode-map)
  (local-set-key [(control c) (control c)] 'tidy-buffer)
  (setq sgml-validate-command "tidy"))
(add-hook 'html-mode-hook 'my-html-mode-hook)

(add-to-list 'load-path "~/.emacs.d/lisp")

(define-key global-map (kbd "RET") 'newline-and-indent)
   (add-hook 'f90-mode-hook (lambda ()
			      (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))

;;(global-set-key "\C-a" 'mark-whole-buffer)
;;(global-set-key "\C-f" 'isearch-forward)
;;(global-set-key "\C-o" 'find-file)
;;(global-set-key "\C-s" 'save-buffer)
;;(global-set-key "\C-w" 'kill-this-buffer)
