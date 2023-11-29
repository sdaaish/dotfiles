;;; INIT-DIRED --- Summary
;;
;; Author: Stig Dahl
;; Created: 2023-11-29
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

;; Dired
(add-hook 'dired-mode-hook
          (lambda ()
            (keymap-set dired-mode-map "'" 'dired-up-directory)
            (dired-hide-details-mode 1)))
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-kill-when-opening-new-dired-buffer t)

(use-package dired-single
  :bind (("C-x d" . dired-single-magic-buffer)
         ([remap dired-find-file] . dired-single-buffer)
         ([remap dired-mouse-find-file-other-window] . dired-single-buffer-mouse)
         ([remap dired-up-directory] . dired-single-up-directory)
         (:map dired-mode-map
               ("'" . dired-single-up-directory))))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))



(provide 'init-dired)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dired.el ends here
