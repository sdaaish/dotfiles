;;; INIT-ORG --- Summary
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


(defun org-jump-to-heading-beginning ()
  "Jump to the beginning of the line of the previous Org heading."
  (interactive)
  (org-back-to-heading)
  (beginning-of-line))

(use-package org
  :bind (:map org-mode-map
              ("C-c C-<tab>" . org-force-cycle-archived)
              ("C-c C-h" . org-tags-view)
              ("C-*" . org-jump-to-heading-beginning))
  :hook (org-mode . (lambda ()
                      (customize-set-variable 'org-use-speed-commands t))))

(use-package org-contrib)


(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
