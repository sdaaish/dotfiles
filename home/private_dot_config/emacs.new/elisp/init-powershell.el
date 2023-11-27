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
  (setq powershell-eldoc-def-files (list (expand-file-name "eldoc/powershell-eldoc.el" user-emacs-directory)))
  (if (eq system-type 'gnu/linux)
      (setq powershell-location-of-exe "/usr/bin/pwsh")
    (setq powershell-location-of-exe "pwsh.exe"))
  ;;  (setq explicit-powershell\.exe-args '("-NoLogo" "-NoProfile" "-Command" "-"))
  ;;  (setq explicit-pwsh\.exe-args '("-NoLogo" "-NoProfile" "-Command" "-"))
  (customize-set-variable 'powershell-indent 4)
  :custom-face (font-lock-variable-name-face ((t (:foreground "#d98026" :weight normal)))))


(use-package ob-powershell
  :custom (ob-powershell-powershell-command "pwsh -NoLogo -NoProfile")
  :config (require 'ob-powershell))


(provide 'init-powershell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-powershell.el ends here
