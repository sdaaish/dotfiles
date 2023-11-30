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

(require 'ob-shell)
(require 'ob-eshell)
(require 'ob-python)
(require 'ob-C)

;; Enable habits
(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-habit t))

;; Diminish org indent
(with-eval-after-load 'org-indent
  (require 'diminish)
  (diminish 'org-indent-mode))

(setq org-todo-keywords
      '((sequence "TODO(t!)" "IN-PROGRESS(p!)" "NEXT(n!)" "WAITING(w@/!)" "|" "DONE(d@)")
        (sequence "IDEA(i!)" "READ(r!)" "|")
        (sequence "REPORT(z!)" "BUG(b!)" "|" "RESOLVED(x@)")
        (sequence "|" "CANCELED(c@)" "DELEGATED(l@)" "SOMEDAY(s!)")))
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "cyan" :weight bold))
        ("IN-PROGRESS" . (:foreground "yellow" :weight bold))
        ("NEXT" . (:foreground "yellow" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("WAITING" . (:foreground "red" :weight bold))
        ("SOMEDAY" . (:foreground "gray" :weight bold))
        ("IDEA" . (:foreground "dark orange" :weight bold))
        ("READ" . (:foreground "dark orange" :weight bold))
        ("BUG" . (:foreground "magenta" :weight bold))
        ("REPORT" . (:foreground "cyan" :weight bold))))
(bind-key "C-c l" 'org-store-link)
(bind-key "C-c a" 'org-agenda)
(bind-key "C-c c" 'org-capture)


(setq my/org-archive-file (expand-file-name "archive/archive.org" my/orgdir))
(when (not (file-exists-p (file-name-directory my/org-archive-file)))
  (make-directory (file-name-directory my/org-archive-file) t))
(setq org-archive-location (concat my/org-archive-file "::datetree/* From %s"))

;; Org agenda custom commands
(setq org-agenda-custom-commands
      '(("c" "Weekly schedule"
         ((agenda ""
                  ((org-agenda-span 10)
                   (org-agenda-start-on-weekday nil)
                   (org-agenda-repeating-timestamp-show-all t)
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
          (alltodo ""
                   ((org-agenda-time-grid nil)
                    (org-deadline-warning-days 90))))
         ((org-agenda-compact-blocks t)))

        ("d" "Upcoming dates"
         ((agenda ""
                  ((org-agenda-entry-types '(:deadline))))
          (agenda ""
                  ((org-agenda-entry-types '(:scheduled)))))
         ((org-agenda-time-grid nil)
          (org-agenda-start-on-weekday nil)
          (org-agenda-span 1)
          (org-deadline-warning-days 14)
          (org-agenda-time-grid nil)
          (org-agenda-compact-blocks t)))

        ("l" "Log for last week"
         ((agenda ""
                  ((org-agenda-span 14)
                   (org-agenda-start-day "-7d")
                   (org-agenda-repeating-timestamp-show-all t)
                   (org-agenda-include-inactive-timestamps t))))
         ((org-agenda-compact-blocks t)))

        ("h" . "Hemma|Huset")
        ("hh" "Agenda and Home-related tasks" tags-todo "Hemma|Huset"
         ((agenda "")
          (org-agenda-sorting-strategy '(priority-up effort-down))))
        ("hc" "Todo" tags-todo "Cyklar"
         ((agenda "")
          (todo "TODO|IN-PROGRESS")
          (org-agenda-sorting-strategy '(priority-up effort-down))))
        ("hf" "Todo" tags-todo "Fordon"
         ((agenda "")
          (todo "TODO|IN-PROGRESS")
          (org-agenda-sorting-strategy '(priority-up effort-down))))
        ("hu" "Todo" tags-todo "Huset"
         ((agenda "")
          (todo "TODO|IN-PROGRESS")
          (org-agenda-sorting-strategy '(priority-up effort-down))))

        ("w" "Agenda and Office-related tasks" tags-todo "work|office"
         ((agenda "")
          (todo "TODO|IN-PROGRESS")
          (org-agenda-sorting-strategy '(priority-up effort-down))))))

;; Org Capture templates
(setq org-capture-templates
      `(("t" "To do items" entry (file+headline my/notes-file "To Do Items")
         "* TODO %^{Description of todo}\nAdded: %U\n\n%?" :prepend t)

        ;; Blog
        ("b" "Blog idea" entry (file+headline my/notes-file "Blog Topics")
         "* IDEA %^{Title} :Blog:\nAdded: %U\n\n%?" :prepend t)

        ;;Links
        ("l" "Link" entry (file+headline my/notes-file "Links")
         "* [[%^C][%^{Title}]]  %^G\nAdded: %U\n%?" :prepend t)

        ;; Notes
        ("n" "Note" entry (file+headline my/notes-file "Notes")
         "* %^{Title} :NOTE:\n%U\n%a\n\n%?" :clock-in t :clock-resume t)

        ;; Idea
        ("i" "Idea" entry (file+headline my/notes-file "Someday")
         "* IDEA %^{Title}\nAdded: %U\n%?" :prepend t)

        ;; Journal
        ("j" "Journal" entry (file+olp+datetree my/diary-file)
         "* %^{Enter title}\n%U\n%?" :clock-in t :clock-resume t)

        ;; Habit
        ("h" "Habit" entry (file+headline my/notes-file "To Do Items")
         ,(concat "* TODO %^{Description of todo}\n"
                  ":PROPERTIES:\n:STYLE: habit\n:ADDED: %U\n:END:\n\n%?") :prepend t)

        ;; Notes for code
        ("c" "Coding stuff")
        ("cc" "note with code" entry (file+headline my/notes-file "Code")
         "* %? \nAdded: %U\n#+begin_src %^{Language?|emacs-lisp|sh|powershell|python|html}\n%^C\n#+end_src\n")
        ("cs" "note with code, source" entry (file+headline my/notes-file "Code")
         "* %? \nAdded: %U\n#+begin_src %^{Language?|emacs-lisp|sh|powershell|python|html}\n%^C\n#+end_src\n%a\n")

        ;; Reports and bugs
        ("r" "Reporting")
        ("rb" "Bug" entry (file+headline my/notes-file "Reports")
         "* BUG %^{Description of bug} %^G\nAdded: %U\n%?")
        ("rr" "Report" entry (file+headline my/notes-file "Reports")
         "* REPORT %^{Description of report} %^G\nAdded: %U\n#+begin_example\n%^C\n#+end_example\n%?")
        ))

;; Calendar
(load "sv-kalender")
(load "sv-kalender-namnsdagar")

;; Org babel
(defun my/org-confirm-babel-evaluate (lang body)
  "Don't confirm code execution for these languages."
  (not (member lang '("python" "emacs-lisp" "shell" "powershell" "perl" "elisp" "eshell"))))


(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
