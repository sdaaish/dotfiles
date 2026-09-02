;;; INIT-ELFEED --- Summary -*- lexical-binding: nil; -*-
;;
;; Author: Stig Dahl
;; Created: 2026-08-10
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Use elfeed as RSS reader
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(use-package elfeed
  :config
  (set-face-attribute 'elfeed-search-unread-title-face nil :weight 'normal :foreground "khaki2"))

(use-package elfeed-org
  :after elfeed
  :custom
  (rmh-elfeed-org-files (list (expand-file-name "elfeed.org" my/orgdir)))
  (rmh-elfeed-org-auto-ignore-invalid-feeds t))


(provide 'init-elfeed)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-elfeed.el ends here
