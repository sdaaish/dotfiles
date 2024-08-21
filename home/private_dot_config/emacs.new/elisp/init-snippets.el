;;; INIT-SNIPPETS --- Summary
;;
;; Author:  Stig Dahl
;; Created: tisdag november 2023-11-28
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



(use-package yasnippet
  :diminish (yas-global-mode yas-minor-mode)
  :init
  (yas-global-mode 1)
  :bind (("C-c y" . hydra-yasnippet/body)
         :map yas-minor-mode-map
         ("C-c i" . yas-expand))
  :commands (yasnippet)

  :config
  (setq yas-snippet-dirs (list (expand-file-name ".config/snippets/" "~")))
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  (yas-reload-all)
  :hook ((prog-mode org-mode) . (lambda ()
                                  (yas-minor-mode 1))))

;; Hydra for YAS snippets
(defhydra hydra-yasnippet (:pre (yas-minor-mode t)
                                :color blue :hint nil)
  "
                      ^YASnippets^
        -----------------------------------
        Actions:    Load/Visit:   Modes:

        _i_nsert     _d_irectory    _c_ompany-yas
        _t_ryout     _f_ile         _g_lobal: %`yas-global-mode
        _n_ew        _l_ist         _m_inor: %`yas-minor-mode
        _e_xtra      _a_ll
  "
  ("c" company-yasnippet)
  ("d" yas-load-directory)
  ("e" yas-activate-extra-mode)
  ("i" yas-insert-snippet)
  ("f" yas-visit-snippet-file :color blue)
  ("n" yas-new-snippet)
  ("t" yas-tryout-snippet)
  ("l" yas-describe-tables)
  ("g" yas-global-mode)
  ("m" yas-minor-mode)
  ("a" yas-reload-all))

;; Load extra snippets
(use-package yasnippet-snippets)

(use-package ivy-yasnippet
  :bind ("C-x y" . ivy-yasnippet))

(defun my/autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-snippets.el ends here
