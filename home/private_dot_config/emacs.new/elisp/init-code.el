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
(use-package zig-mode)

(use-package zig-ts-mode
  :straight (zig-ts-mode :type git :host codeberg :repo "meow_king/zig-ts-mode")
  :config (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-ts-mode))
  :bind (:map zig-ts-mode-map
              ("C-c C-b" . zig-compile)
              ("C-c C-r" . zig-run)
              ("C-c C-f" . zig-format-buffer)
              ("C-c C-t" . zig-test-buffer))
  :hook (zig-ts-mode . (lambda() (zig-format-on-save-mode 1))))

;; devdocs.io viewer
(use-package devdocs
  :bind ("C-," . devdocs-lookup)
  ("C-h D" . hydra-devdocs/body)
  (:map devdocs-mode-map
        ("b" . 'devdocs-go-back)
        ("f" . 'devdocs-go-forward)
        ("M-p" . 'devdocs-peruse)
        ("SPC" . 'scroll-up-command)
        ("S-SPC" . 'scroll-down-command))
  :hook
  (emacs-lisp-mode . (lambda () (setq-local devdocs-current-docs '("elisp"))))
  ((python-mode python-ts-mode) . (lambda () (setq-local devdocs-current-docs '("python~3.12"))))
  ((zig-mode zig-ts-mode) . (lambda () (setq-local devdocs-current-docs '("zig"))))
  ((go-mode go-ts-mode) . (lambda () (setq-local devdocs-current-docs '("go")))))


;; Remember to copy the libraries to the correct folder and rename them.
(use-package treesit
  :straight (:type built-in)
  :config (add-to-list 'treesit-extra-load-path (expand-file-name "tree-sitter/" "~/.config"))
  (setq major-mode-remap-alist
        '((bash-mode . bash-ts-mode)
          (css-mode . css-ts-mode)
          (csv-mode . csv-ts-mode)
          (dockerfile-mode . dockerfile-ts-mode)
          (go-mode . go-ts-mode)
          (html-mode . html-ts-mode)
          (mermaid-mode . mermaid-ts-mode)
          (js2-mode . js-ts-mode)
          (json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (yaml-mode . yaml-ts-mode)
          (zig-mode . zig-ts-mode))))

;; Leave this out for now
;;(use-package tree-sitter-langs)

;; Flymake
(with-eval-after-load 'flymake
  (bind-key "M-n" #'flymake-goto-next-error 'flymake-mode-map)
  (bind-key "M-p" #'flymake-goto-prev-error 'flymake-mode-map)
  (bind-key "C-c m" #'flymake-show-buffer-diagnostics 'flymake-mode-map))

(provide 'init-code)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-code.el ends here
