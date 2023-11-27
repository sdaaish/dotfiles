(use-package yasnippet
  :config (yas-reload-all)
  (add-to-list 'yas-snippet-dirs (expand-file-name ".config/snippets/" "~"))
  :hook ((prog-mode org-mode) . (lambda ()
                                  (yas-minor-mode 1))))

(use-package yasnippet-snippets)

(use-package ivy-yasnippet
  :bind ("C-x y" . ivy-yasnippet))

(defun my/autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas/expand-snippet (buffer-string) (point-min) (point-max)))

(use-package autoinsert
  :custom
  (auto-insert 'other)
  (auto-insert-query nil)
  (auto-insert-directory "~/.config/templates/")
  (auto-insert-alist '((("\\.sh\\'" . "Shell script") . ["template.sh" my/autoinsert-yas-expand])
                       (("\\.el\\'" . "Emacs Lisp") . ["template.el" my/autoinsert-yas-expand])
                       (("\\.go\\'" . "Go script") . ["template.go" my/autoinsert-yas-expand])
                       (("\\.py\\'" . "Python script") . ["template.py" my/autoinsert-yas-expand])
                       (("\\.ps1\\'" . "Powershell script") . ["template.ps1" my/autoinsert-yas-expand])
                       (("\\.psm1\\'" . "Powershell Module") . ["template.psm1" my/autoinsert-yas-expand])))
  :config (auto-insert-mode 1))

(provide 'init-snippets)
