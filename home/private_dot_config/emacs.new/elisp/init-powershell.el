;;; INIT-POWERSHELL --- Summary
;;
;; Author: Stig Dahl
;; Created: 27 November 2023-11-27
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

(use-package powershell
  :config
  (if (eq system-type 'gnu/linux)
      (setq powershell-location-of-exe "/usr/bin/pwsh")
    (setq powershell-location-of-exe "pwsh.exe"))
  ;;  (setq explicit-powershell\.exe-args '("-NoLogo" "-NoProfile" "-Command" "-"))
  ;;  (setq explicit-pwsh\.exe-args '("-NoLogo" "-NoProfile" "-Command" "-"))
  (setq powershell-eldoc-def-files (expand-file-name "eldoc/powershell-eldoc.el" (getenv "HOME")))

  :custom
  (powershell-indent 2)

  :custom-face
  (font-lock-variable-name-face ((t (:foreground "#d98026" :weight normal)))))


(use-package ob-powershell
  :custom (ob-powershell-powershell-command "pwsh -NoLogo -NoProfile")
  :config (require 'ob-powershell))

;; Install Treesitter for PowerShell
;; Need to compile https://github.com/airbus-cert/tree-sitter-powershell
(use-package powershell-ts-mode
  :straight (:host github :repo "dmille56/powershell-ts-mode")
  :config
  ;; Optional: if you want to disable top-level vars from being shown in imenu
  (setq powershell-ts-enable-imenu-top-level-vars nil))

(provide 'init-powershell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-powershell.el ends here
