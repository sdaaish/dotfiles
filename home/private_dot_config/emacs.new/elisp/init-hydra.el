;;; INIT-HYDRA --- Summary
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

(use-package hydra)

;; Hydra for changes to display
(defhydra hydra-toggle (:color pink :timeout 4)
  "
        _a_ abbrev-mode:       %`abbrev-mode
        _c_ flycheck-mode:     %`flycheck-mode
        _d_ debug-on-error:    %`debug-on-error
        _e_ editorconfig-mode: %`editorconfig-mode
        _f_ auto-fill-mode:    %`auto-fill-function
        _h_ hs-minor-mode:     %`hs-minor-mode
        _t_ truncate-lines:    %`truncate-lines
        _v_ visual-lines:      %`visual-line-mode
        _w_ whitespace-mode:   %`whitespace-mode
        _l_ linenumber:        %`display-line-numbers-mode
        _r_ relative-lines:    %`display-line-numbers-type
        _R_ rainbow-delimiters %`rainbow-delimiters-mode
        _C_ rainbow-mode       %`rainbow-mode
        _q_ Quit
  "
  ("a" abbrev-mode nil)
  ("c" flycheck-mode nil)
  ("C" rainbow-mode nil)
  ("d" toggle-debug-on-error nil)
  ("e" editorconfig-mode nil)
  ("f" auto-fill-mode nil)
  ("h" hs-minor-mode nil)
  ("l" my/line-number-t nil)
  ("r" my/line-number-relative nil)
  ("R" rainbow-delimiters-mode nil)
  ("t" toggle-truncate-lines nil)
  ("v" visual-line-mode nil)
  ("w" whitespace-mode nil)
  ("q" nil "cancel" :color blue))
(bind-key "C-c v" 'hydra-toggle/body)

(defun my/line-number-relative ()
  "Display relative line numbers."
  (interactive)
  (setq-local display-line-numbers-type 'visual)
  (display-line-numbers-mode 'toggle))

(defun my/line-number-t ()
  "Display absolute line numbers."
  (interactive )
  (setq-local display-line-numbers-type t)
  (display-line-numbers-mode 'toggle))

;; Hydra for Emacs config and macro
(defhydra hydra-config-files (:color blue :columns 3)
  "Emacs config files"
  ("c" (find-file custom-file) "Emacs custom file")
  ("e" (find-file (expand-file-name "early-init.el" user-emacs-directory)) "early-init.el")
  ("i" (find-file user-init-file) "init.el")
  ("k" my/server-shutdown "Save&kill emacs")
  ("l" (dired-at-point (expand-file-name "elisp/" user-emacs-directory)) "Elisp directory")
  ("m" (start-kbd-macro) "Start kbd macro")
  ("r" (load-file user-init-file) "Reload emacs")
  ("s" (end-kbd-macro) "Stop kbd macro")
  ("q" nil "cancel"))
(bind-key "<f8> i" 'hydra-config-files/body)

(defun my/server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; Hydra for straight package manager
(defhydra hydra-straight-helper (:hint nil)
  "
  _c_heck all       |_f_etch all     |_m_erge all      |_n_ormalize all   |p_u_sh all
  _C_heck package   |_F_etch package |_M_erge package  |_N_ormlize package|p_U_sh package
  ----------------^^+--------------^^+---------------^^+----------------^^+------------||_q_uit||
  _r_ebuild all     |_p_ull all      |_v_ersions freeze|_w_atcher start   |_g_et recipe
  _R_ebuild package |_P_ull package  |_V_ersions thaw  |_W_atcher quit    |prun_e_ build
  _h_ Describe package"
  ("c" straight-check-all)
  ("C" straight-check-package)
  ("r" straight-rebuild-all)
  ("R" straight-rebuild-package)
  ("f" straight-fetch-all)
  ("F" straight-fetch-package)
  ("p" straight-pull-all)
  ("P" straight-pull-package)
  ("m" straight-merge-all)
  ("M" straight-merge-package)
  ("n" straight-normalize-all)
  ("N" straight-normalize-package)
  ("u" straight-push-all)
  ("U" straight-push-package)
  ("v" straight-freeze-versions)
  ("V" straight-thaw-versions)
  ("w" straight-watcher-start)
  ("W" straight-watcher-quit)
  ("g" straight-get-recipe)
  ("e" straight-prune-build)
  ("h" describe-package)
  ("q" nil))
(global-set-key (kbd "C-h P") 'hydra-straight-helper/body)

;; Hydra for Ivy and Counsel
(defhydra hydra-ivy (:color blue)
  "Counsel search commands"
  ("a" counsel-ag "Ag")
  ("e" elgrep "elGrep")
  ("f" counsel-fzf "FzF")
  ("g" counsel-grep "Grep")
  ("j" counsel-git-grep "Git Grep")
  ("r" counsel-rg "RipGrep")
  ("s" counsel-search "Search Engine"))

;; Switch between Modus themes
(defhydra hydra-theme-chooser (:color blue :columns 2 :timeout 3)
  "Switch between Modus themes"
  ("v t" (modus-themes-select 'modus-vivendi-tinted) "Vivendi Tinted")
  ("o t" (modus-themes-select 'modus-operandi-tinted) "Operandi Tinted")
  ("v v" (modus-themes-select 'modus-vivendi) "Vivendi")
  ("o o" (modus-themes-select 'modus-operandi) "Operandi")
  ("t" (modus-themes-toggle) "Toggle theme")
  ("q" nil "Quit"))

(bind-key "S-<f5>" 'hydra-theme-chooser/body)

;; A hydra for devdocs.io
(defhydra hydra-devdocs (:color blue :columns 2)
  "Lookup DevDocs.IO information"
  ("i" devdocs-install "Install language")
  ("l" devdocs-lookup "Lookup")
  ("p" devdocs-peruse "Open first page")
  ("s" devdocs-search "Search devdocs web")
  ("u" devdocs-update-all "Update all docs")
  ("q" nil "Quit"))

;; Hydra for hs-minor-mode commands
(defhydra hydra-hideshow (:color blue :columns 2)
  "Hide and show code and comments blocks"
  ("b" hs-hide-block "Hide block")
  ("c" hs-show-block "Show block")
  ("l" hs-hide-level "Hide level")
  ("h" hs-hide-all "Hide all")
  ("s" hs-show-all "Show all")
  ("t" hs-toggle-hiding "Toggle hiding")
  ("q" nil "Quit"))

(provide 'init-hydra)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-hydra.el ends here
