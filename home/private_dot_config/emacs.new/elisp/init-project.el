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
               _ff_: file dwim       _g_: update gtags      _b_: switch to buffer  _x_: remove known project
               _fd_: file curr dir   _m_: multi-occur       _k_: Kill all buffers  _X_: cleanup non-existing
                _r_: recent file                                               ^^^^_z_: cache current
                _d_: dir

              "
  ("a"   projectile-ag)
  ("b"   projectile-switch-to-buffer)
  ("c"   projectile-invalidate-cache)
  ("d"   projectile-find-dir)
  ("s-f" projectile-find-file)
  ("F"   projectile-find-file)
  ("ff"  projectile-find-file-dwim)
  ("fd"  projectile-find-file-in-directory)
  ("g"   ggtags-update-tags)
  ("i"   projectile-ibuffer)
  ("k"   projectile-kill-buffers)
  ("m"   projectile-multi-occur)
  ("p"   projectile-switch-project "switch project")
  ("r"   projectile-recentf)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("z"   projectile-cache-current-file)
  ("'"   hydra-projectile-other-window/body "open other window")
  ("q"   nil "cancel" :color blue))

(use-package projectile
  :diminish projectile-mode
  :init (when (file-directory-p "~/repos")
          (setq projectile-project-search-path '("~/repos" "~/code" "~/work")))
  :config
  (projectile-mode t)
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action 'projectile-dired)
  :bind (("C-c p" . hydra-projectile/body)
         (:map projectile-mode-map
               ("C-c P" . projectile-command-map))))

(use-package counsel-projectile
  :init (counsel-projectile-mode 1))


(provide 'init-project)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-project.el ends here
