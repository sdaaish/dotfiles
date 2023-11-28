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
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         (:map org-mode-map
               ("C-c C-<tab>" . org-force-cycle-archived)
               ("C-c C-h" . org-tags-view)
               ("C-*" . org-jump-to-heading-beginning)))
  
  :hook (org-mode . (lambda ()
                      (customize-set-variable 'org-use-speed-commands t)))
  :custom
  ;; Org agenda
  (org-agenda-dim-blocked-tasks t)
  (org-agenda-files my/org-agenda-files)
  (org-agenda-include-diary nil)
  (org-agenda-skip-deadline-prewarning-if-scheduled t)
  (org-agenda-skip-scheduled-if-deadline-is-shown "repeated-after-deadline")
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-span 14)

  (org-confirm-babel-evaluate 'my/org-confirm-babel-evaluate)
  (org-default-notes-file my/notes-file)
  (org-directory my/orgdir)
  (org-ellipsis "  ")
  (org-enable-priority-commands nil)
  (org-enforce-todo-checkbox-dependencies t)
  (org-enforce-todo-dependencies t)
  (org-export-backends '(ascii html latex odt org))
  (org-export-coding-system 'utf-8)
  (org-export-use-babel t)
  (org-export-with-sub-superscripts '{})
  (org-fast-tag-selection-single-key t)
  (org-habit-graph-column 60)
  (org-habit-show-habits-only-for-today t)
  (org-hide-emphasis-markers t)
  (org-html-validation-link nil)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-log-redeadline 'time)
  (org-log-refile 'time)
  (org-log-reschedule 'time)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-targets '((nil :maxlevel . 3)(org-agenda-files :maxlevel . 3)))
  (org-refile-use-outline-path 'file)
  (org-src-preserve-indentation t)
  (org-use-fast-todo-selection t)
  (org-use-sub-superscripts '{})

  ;; Org clock settings
  (org-clock-clocked-in-display 'both)
  (org-clock-display-default-range 'thisweek)
  (org-clock-history-length 35)
  (org-clock-idle-time 60)
  (org-clock-in-resume t)
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-persist t)

  :config
  (org-clock-persistence-insinuate))

(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-habit t))

(use-package org-contrib)


(load "sv-kalender")
(load "sv-kalender-namnsdagar")


(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
