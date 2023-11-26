(use-package chezmoi
  :config (setq-default chezmoi-template-display-p t)
  :bind
  ("C-c C f" .  #'chezmoi-find)
  ("C-c C s" .  #'chezmoi-write)
  :hook (chezmoi-mode . (lambda ()
                               (if chezmoi-mode
                                   (add-to-list 'company-backends 'chezmoi-company-backend)
                                 (delete 'chezmoi-company-backend 'company-backends)))))

(provide 'init-chezmoi)
