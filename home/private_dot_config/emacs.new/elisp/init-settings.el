;; Setup constants and variables for packages.

(defun my/setup-dirs(dir basedir)
  "Returns an expanded path from DIR and BASEDIR"
  (expand-file-name dir basedir))

;; Org mode directory
(cond ((getenv "ORG_DIR")
       (defconst my/orgdir (expand-file-name "Org/" (getenv "ORG_DIR"))))
      ((getenv "OneDriveCommercial")
       (defconst my/orgdir (expand-file-name "Org/" (getenv "OneDriveCommercial"))))
      ((getenv "OneDrive")
       (defconst my/orgdir (expand-file-name "Org/" (getenv "OneDrive"))))
      (t (defconst my/orgdir (expand-file-name "Org/" (getenv "HOME")))))

(message "Setup Org home directory to: %s" my/orgdir)
(when (not (file-exists-p my/orgdir))
  (make-directory my/orgdir))

;; Denote mode directory
(cond ((getenv "DENOTE_DIR")
       (defconst my/denotedir (expand-file-name "notes/" (getenv "DENOTE_DIR"))))
      ((getenv "OneDriveCommercial")
       (defconst my/denotedir (expand-file-name "notes/" (getenv "OneDriveCommercial"))))
      ((getenv "OneDrive")
       (defconst my/denotedir (expand-file-name "notes/" (getenv "OneDrive"))))
      (t (defconst my/denotedir (expand-file-name "notes/" (getenv "HOME")))))

(message "Setup Denote home directory to: %s" my/denotedir)
(when (not (file-exists-p my/denotedir))
  (make-directory my/denotedir))

;; Use work version of Onedrive if it exists
(if (getenv "OneDriveCommercial")
    (defconst my/onedrive-dir (getenv "OneDriveCommercial"))
  (defconst my/onedrive-dir (getenv "OneDrive")))

;; Local settings per device
(setq my/private-elfile (expand-file-name "local/personal.el" user-emacs-directory))
(if (file-exists-p my/private-elfile)
    (load-file my/private-elfile))

;; Org agenda settings
(defconst my/notes-file (expand-file-name "notes.org" my/orgdir))
(defconst my/diary-file (expand-file-name "diary.org" my/orgdir))
(defconst my/org-agenda-files (expand-file-name ".agenda-files" my/orgdir))
(defconst my/org-target-files (expand-file-name ".target-files" my/orgdir))

;; Store auto-saved bufferes here
(defconst my/auto-save-dir (expand-file-name ".cache/autosave/" user-emacs-directory))
(when (not (file-exists-p my/auto-save-dir))
  (make-directory my/auto-save-dir t))

(provide 'init-settings)
