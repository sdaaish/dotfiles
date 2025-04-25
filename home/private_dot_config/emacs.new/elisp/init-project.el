;;; INIT-PROJECT --- Summary
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

(defhydra hydra-projectile-other-window (:color teal)
  "projectile-other-window"
  ("f"  projectile-find-file-other-window        "file")
  ("g"  projectile-find-file-dwim-other-window   "file dwim")
  ("d"  projectile-find-dir-other-window         "dir")
  ("b"  projectile-switch-to-buffer-other-window "buffer")
  ("q"  nil                                      "cancel" :color blue))

(defhydra hydra-projectile (:color teal :hint nil)
  "
                   PROJECTILE: %(projectile-project-root)

                   Find File            Search/Tags          Buffers                Cache
              ------------------------------------------------------------------------------------------
                _F_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
               _ff_: file dwim       _g_: grep              _b_: switch to buffer  _x_: remove known project
               _fd_: file curr dir   _j_: git grep          _k_: Kill all buffers  _X_: cleanup non-existing
                _r_: recent file     _m_: multi-occur                                ^^^^_z_: cache current
                _d_: dir             _r_:rg                 _C_:Org capture
                                   ^^^^_T_: update gtags      _A_:Org Agenda
              "
  ("a"   counsel-projectile-ag)
  ("A"   counsel-projectile-org-agenda)
  ("b"   counsel-projectile-switch-to-buffer)
  ("c"   projectile-invalidate-cache)
  ("C"   counsel-projectile-org-capture)
  ("d"   counsel-projectile-find-dir)
  ("s-f" counsel-projectile-find-file)
  ("F"   counsel-projectile-find-file)
  ("ff"  counsel-projectile-find-file-dwim)
  ("fd"  projectile-find-file-in-directory)
  ("g"   counsel-projectile-grep)
  ("i"   projectile-ibuffer)
  ("j"   counsel-projectile-git-grep)
  ("k"   projectile-kill-buffers)
  ("m"   projectile-multi-occur)
  ("p"   projectile-switch-project "switch project")
  ("r"   counsel-projectile-rg)
  ("R"   projectile-recentf)
  ("T"   ggtags-update-tags)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("z"   projectile-cache-current-file)
  ("'"   hydra-projectile-other-window/body "open other window")
  ("q"   nil "cancel" :color blue))

(use-package projectile
  :diminish projectile-mode
  :init (when (file-directory-p "~/repos")
          (setq projectile-project-search-path '(("~/repos" . 1) ("~/code" . 1) ("~/work" . 1))))
  :config
  (projectile-mode t)
  ;;  (add-to-list 'projectile-ignored-projects "~")

  :custom
  (projectile-completion-system 'ivy)
  (projectile-switch-project-action 'projectile-dired)
  (projectile-indexing-method 'alien)
  :bind (("C-c p" . hydra-projectile/body)
         (:map projectile-mode-map
               ("C-c P" . projectile-command-map))))

(use-package counsel-projectile
  :init (counsel-projectile-mode 1))


(provide 'init-project)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-project.el ends here
