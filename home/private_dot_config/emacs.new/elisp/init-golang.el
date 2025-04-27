;; Go lang settings for eglot.
(require 'project)

(use-package go-mode
  :no-require go-ts-mode
  :after eglot

  :config
  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))

  (defun go-compile()
    "Compile the current buffer"
    (interactive)
    (compile (concat "go run " (file-name-nondirectory (buffer-file-name)))))

  (defun go-build()
    "Build the current buffer"
    (interactive)
    (compile (concat "go build " (file-name-nondirectory (buffer-file-name)))))

  :bind (:map go-ts-mode-map
              ("C-c C-c" . 'go-compile)
              ("C-c C-r" . 'recompile)
              ("C-c C-b" . 'go-build)
              ("C-c C-a" . 'go-import-add))

  :hook ((go-mode go-ts-mode) . 'my/line-number-relative))


(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(add-hook 'project-find-functions #'project-find-go-module)

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

;; See https://jeffbowman.writeas.com/crafted-emacs-example-adding-go
(use-package go-eldoc
  :hook ((go-mode go-ts-mode) . 'go-eldoc-setup))

(provide 'init-golang)
