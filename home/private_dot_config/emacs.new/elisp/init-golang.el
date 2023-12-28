;; Go lang settings for eglot.
(require 'project)

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(use-package go-mode
  :config (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode)))

(add-hook 'project-find-functions #'project-find-go-module)

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(provide 'init-golang)
