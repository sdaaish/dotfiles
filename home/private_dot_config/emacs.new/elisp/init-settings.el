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
      (defconst my/orgdir (expand-file-name "Org/" (getenv "HOME"))))

(message "Setup Org home directory to: %s" my/orgdir)

;; Denote mode directory
(cond ((getenv "DENOTE_DIR")
       (defconst my/denotedir (expand-file-name "notes/" (getenv "DENOTE_DIR"))))
      ((getenv "OneDriveCommercial")
       (defconst my/denotedir (expand-file-name "notes/" (getenv "OneDriveCommercial"))))
      ((getenv "OneDrive")
       (defconst my/denotedir (expand-file-name "notes/" (getenv "OneDrive"))))
      (defconst my/denotedir (expand-file-name "notes/" (getenv "HOME"))))

(message "Setup Denote home directory to: %s" my/denotedir)

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

(provide 'init-settings)
