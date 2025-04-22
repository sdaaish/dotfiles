;; Go lang settings for eglot.
(require 'project)

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(use-package go-mode
  :config (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
  :hook ((go-mode go-ts-mode) . 'my/line-number-relative))

(add-hook 'project-find-functions #'project-find-go-module)

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

;; See https://jeffbowman.writeas.com/crafted-emacs-example-adding-go
(use-package go-eldoc
  :hook ((go-mode go-ts-mode) . 'go-eldoc-setup))

(provide 'init-golang)
