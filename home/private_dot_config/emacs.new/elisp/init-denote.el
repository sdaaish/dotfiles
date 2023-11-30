;;; INIT-DENOTE --- Summary
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

(defhydra hydra-denote (:color teal :hint nil)
  "
             Note                         Type                       Rename
             -------------------------------------------------------------------------------
             _n_: New                     _i_: Link                  _r_: Rename file
             _a_: New from region         _I_: Add links             _R_: Rename using Front Matter
             _j_: Journal                 _m_: Add missing links     _x_: Split Org subtree
             _s_: Subdirectory            _b_: Backlink              ^
             _N_: Choose type            _ff_: Find File             _q_: Quit
             _d_: Date                   _fb_: Find backlink
             _t_: Template                _F_: Open Denote directory
    "
  ("a"  my/denote-create-new-note-from-region)
  ("j"  my/denote-journal)
  ("n"  #'denote)
  ("N"  #'denote-type)
  ("d"  #'denote-date)
  ("s"  #'denote-subdirectory)
  ("t"  #'denote-template)
  ("i"  #'denote-link)
  ("I"  #'denote-link-add-links)
  ("m"  #'denote-link-add-missing-links)
  ("b"  #'denote-link-backlinks)
  ("ff"  #'denote-link-find-file)
  ("fb"  #'denote-link-find-backlink)
  ("F"  (find-file denote-directory))
  ("r"  #'denote-rename-file)
  ("R"  #'denote-rename-file-using-front-matter)
  ("x"  my/denote-split-org-subtree :base-map org-mode-map)
  ("q"  nil "cancel" :color blue))

(use-package denote
  :commands (hydra-denote/body denote)
  :custom
  (denote-directory my/denotedir)
  (denote-dired-directories (list denote-directory))
  (denote-date-prompt-use-org-read-date t)
  (denote-known-keywords '("emacs" "project" "powershell"))
  :hook
  ((dired-mode . denote-dired-mode-in-directories)
   (find-file . denote-link-buttonize-buffer))
  :bind (("C-c n" . hydra-denote/body)
         (:map dired-mode-map
               ("C-c C-d C-i" . #'denote-link-dired-marked-notes)
               ("C-c C-d C-r" . #'denote-dired-rename-marked-files)
               ("C-c C-d C-R" . #'denote-dired-rename-marked-files-using-front-matter))))

(defun my/denote-journal ()
  "Create an entry tagged 'journal', while prompting for a title."
  (interactive)
  (denote
   (denote--title-prompt)
   '("journal")))

(with-eval-after-load 'org-capture
  (setq denote-org-capture-specifiers "%l\n%i\n%?")
  (add-to-list 'org-capture-templates
               '("d" "New denote, note with Denote)" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))

(defun my/denote-split-org-subtree ()
  "Create new Denote note as an Org file using current Org subtree."
  (interactive)
  (let ((text (org-get-entry))
        (heading (org-get-heading :no-tags :no-todo :no-priority :no-comment))
        (tags (org-get-tags)))
    (delete-region (org-entry-beginning-position) (org-entry-end-position))
    (denote heading tags 'org)
    (insert text)))

(defun my/denote-create-new-note-from-region (beg end)
  "Create note whose contents include the text between BEG and END.
Prompt for title and keywords of the new note."
  (interactive "r")
  (if-let (((region-active-p))
           (text (buffer-substring-no-properties beg end)))
      (progn
        (denote (denote--title-prompt) (denote--keywords-prompt))
        (insert text))
    (user-error "No region is available")))

(with-eval-after-load 'denote
  (customize-set-variable 'denote-templates
                          `((memo . ,(concat "* Content"
                                             "\n\n"
                                             "* Topics"
                                             "\n\n"))
                            (meeting-notes . ,(concat "* Notes\n"
                                                      "- Subject:\n"
                                                      "- Date:\n"
                                                      "- Attendees:\n"))))
  (when (file-readable-p (expand-file-name "local/denote-templates.el" user-emacs-directory))
    (load-file (expand-file-name "local/denote-templates.el" user-emacs-directory))))


(provide 'init-denote)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-denote.el ends here
