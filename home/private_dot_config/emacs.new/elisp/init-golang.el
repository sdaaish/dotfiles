;; Go lang settings for eglot.
(require 'project)

(use-package go-mode)

(use-package go-ts-mode
  :config
  (defun go-compile()
    "Compile the current buffer"
    (interactive)
    (compile (concat "go run " (file-name-nondirectory (buffer-file-name)))))

  (defun go-build()
    "Build the current buffer"
    (interactive)
    (compile (concat "go build " (file-name-nondirectory (buffer-file-name)))))

  (my/line-number-relative)

  :bind (:map go-ts-mode-map
              ("C-c C-c" . 'go-compile)
              ("C-c C-r" . 'recompile)
              ("C-c C-b" . 'go-build)
              ("C-c C-a" . 'go-import-add)
              ("C-c C-o" . 'eglot-code-action-organize-imports)
              ("C-c C-h" . 'hydra-golang/body))

  :hook (prog-mode . my/line-number-relative))


(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(add-hook 'project-find-functions #'project-find-go-module)

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

;; See https://jeffbowman.writeas.com/crafted-emacs-example-adding-go
(use-package go-eldoc
  :hook ((go-mode go-ts-mode) . go-eldoc-setup))

(provide 'init-golang)
