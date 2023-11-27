;; Setup constants and variables for packages.

(defun my/setup-dirs(dir basedir)
  "Returns an expanded path from DIR and BASEDIR"
  (expand-file-name dir basedir))

;; Org mode directory
(cond ((getenv "ORG_DIR")
       (defconst orgdir (expand-file-name "Org/" (getenv "ORG_DIR"))))
      ((getenv "OneDriveCommercial")
       (defconst orgdir (expand-file-name "Org/" (getenv "OneDriveCommercial"))))
      ((getenv "OneDrive")
       (defconst orgdir (expand-file-name "Org/" (getenv "OneDrive"))))
      (defconst orgdir (expand-file-name "Org/" (getenv "HOME"))))

(message "Setup Org home directory to: %s" orgdir)

;; Denote mode directory
(cond ((getenv "DENOTE_DIR")
       (defconst denotedir (expand-file-name "notes/" (getenv "DENOTE_DIR"))))
      ((getenv "OneDriveCommercial")
       (defconst denotedir (expand-file-name "notes/" (getenv "OneDriveCommercial"))))
      ((getenv "OneDrive")
       (defconst denotedir (expand-file-name "notes/" (getenv "OneDrive"))))
      (defconst denotedir (expand-file-name "notes/" (getenv "HOME"))))

(message "Setup Denote home directory to: %s" denotedir)


;; Local settings per device
(setq my/private-elfile (expand-file-name "personal.el" user-emacs-directory))
(if (file-exists-p my/private-elfile)
    (load-file my/private-elfile))

(provide 'init-settings)
