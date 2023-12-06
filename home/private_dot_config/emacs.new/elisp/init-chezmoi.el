(use-package chezmoi
  :config (setq-default chezmoi-template-display-p t)
  :bind
  ("C-c C" .  #'hydra-chezmoi/body)
  :hook (chezmoi-mode . (lambda ()
                          (if chezmoi-mode
                              (add-to-list 'company-backends 'chezmoi-company-backend)
                            (delete 'chezmoi-company-backend 'company-backends)))))

(defhydra hydra-chezmoi (:color blue :column 2)
  "Chezmoi commands."
  ("f" chezmoi-find "Find")
  ("w" chezmoi-write "Save")
  ("o" chezmoi-open-other "Open")
  ("s" chezmoi-sync-files "Sync")
  ("d" chezmoi-diff "Diff"))


(provide 'init-chezmoi)
