;;; INIT-CODE --- Summary
;;
;; Author: Stig Dahl
;; Created: 2023-12-09
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

(use-package editorconfig
  :diminish)

;; Zig code support
(use-package zig-mode
  :config (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode)))

;; (use-package zig-ts-mode
;;   :disabled
;;   :straight (zig-ts-mode :type git :host codeberg :repo "meow_king/zig-ts-mode")
;;   :bind (:map zig-ts-mode-map
;;               ("C-c C-b" . zig-compile)
;;               ("C-c C-r" . zig-run)
;;               ("C-c C-f" . zig-format-buffer)
;;               ("C-c C-t" . zig-test-buffer))
;;   :hook (zig-ts-mode . (lambda() (zig-format-on-save-mode 1))))

;; devdocs.io viewer

(use-package devdocs
  :init
  (defun scale-shr-text (scale-factor)
    "Scale the font size in shr-mode by SCALE-FACTOR."
    (let ((new-height (truncate (* scale-factor (face-attribute 'shr-text :height)))))
      (set-face-attribute 'shr-text nil :height new-height)))

  (defun scale-shr-text-increase ()
    (interactive)
    (scale-shr-text 1.1))

  (defun scale-shr-text-decrease ()
    (interactive)
    (scale-shr-text 0.9))

  (add-to-list 'display-buffer-alist
               '("\\*devdocs\\*"
                 display-buffer-in-side-window
                 (side . right)
                 (slot . 3)
                 (dedicated . t)
                 (body-function . my/body-select-window)))

  :custom-face (shr-text ((t (:height 166))))

  :bind
  ("C-," . devdocs-lookup)
  ("C-h D" . hydra-devdocs/body)
  (:map devdocs-mode-map
        ("b" . devdocs-go-back)
        ("f" . devdocs-go-forward)
        ("M-p" . devdocs-peruse)
        ("SPC" . scroll-up-command)
        ("S-SPC" . scroll-down-command)
        ("C-<kp-add>" . scale-shr-text-increase)
        ("C-<kp-subtract>" . scale-shr-text-decrease))


  :hook
  (emacs-lisp-mode . (lambda () (setq-local devdocs-current-docs '("elisp"))))
  ((python-mode python-ts-mode inferior-python-mode) . (lambda () (setq-local devdocs-current-docs '("python~3.12"))))
  ((zig-mode zig-ts-mode) . (lambda () (setq-local devdocs-current-docs '("zig"))))
  ((go-mode go-ts-mode) . (lambda () (setq-local devdocs-current-docs '("go")))))


;; Remember to copy the libraries to the correct folder and rename them.
(use-package treesit
  :straight (:type built-in)
  :config (add-to-list 'treesit-extra-load-path (expand-file-name "tree-sitter/" "~/.config")))

;; Active tree-sitter for all major modes
(use-package treesit-auto
  :custom (treesit-auto-install nil)
  :config (global-treesit-auto-mode)
  (treesit-auto-add-to-auto-mode-alist 'all))

;; Leave this out for now
;;(use-package tree-sitter-langs)

;; Flymake
(use-package flymake
  :straight (:type built-in)
  :bind (:map flymake-mode-map
              ("M-n" . #'flymake-goto-next-error)
              ("M-p" . #'flymake-goto-prev-error)
              ("C-c m" . #'flymake-show-buffer-diagnostics))
  :init
  (add-to-list 'display-buffer-alist
               '("^\\*Flymake diagnostics"
                 (display-buffer-reuse-window display-buffer-pop-up-window)
                 (window-height . 8)))
  :hook
  (go-ts-mode . (lambda()
                  (flymake-mode t)
                  (flymake-show-buffer-diagnostics))))

;; Jinja 2 formatting for Go and Ansible
(use-package jinja2-mode
  :config (setq jinja2-enable-indent-on-save t))


;; Copilot
(use-package gptel
  :bind ("C-c g" . gptel)
  (:map gptel-mode-map
        ("C-c g" . hydra-gptel/body)
        ("C-c C-c" . gptel-send)
        ("C-c m" . gptel-menu)
        ("C-c r" . gptel-rewrite))
  (:map dired-mode-map
        ("a" . gptel-add-file))

  :custom
  (gptel-model 'gpt-5-mini)
  (gptel-default-mode 'org-mode)

  :config
  (setq gptel-backend (gptel-make-gh-copilot "Copilot")))


;; AI code
(use-package ai-code
  :custom
  (ai-code-selected-backend 'github-copilot-cli)
  (ai-code-backends-infra-terminal-backend 'vterm)
  (ai-code-auto-test-type 'ask-me)
  (ai-code-prompt-filepath-completion-mode 1)

  :hook (magit . ai-code-magit-setup-transients))

(provide 'init-code)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-code.el ends here
