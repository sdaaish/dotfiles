;; Optional: load other packages before eglot to enable eglot integrations.

(use-package eglot)
(add-hook 'go-mode-hook 'eglot-ensure)

(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'go-mode-hook #'eglot-format-buffer-on-save)

(provide 'init-eglot)
