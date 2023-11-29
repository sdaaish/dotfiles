;;; INIT-MAGIC --- Summary
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

;; The extremely useful git interface
(use-package magit
  :diminish magit-status
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  (if (eq system-type 'windows-nt)
      (setq-default with-editor-emacsclient-executable "emacsclientw.exe")
    (setq-default with-editor-emacsclient-executable "emacsclient"))
  :custom (magit-submodule-list-columns
           '(("Path" 35 magit-modulelist-column-path nil)
             ("Version" 25 magit-repolist-column-version
              ((:sort magit-repolist-version<)))
             ("Branch" 15 magit-repolist-column-branch nil)
             ("B<U" 3 magit-repolist-column-unpulled-from-upstream
              ((:right-align t)
               (:sort <)))
             ("B>U" 3 magit-repolist-column-unpushed-to-upstream
              ((:right-align t)
               (:sort <)))
             ("B<P" 3 magit-repolist-column-unpulled-from-pushremote
              ((:right-align t)
               (:sort <)))
             ("B>P" 3 magit-repolist-column-unpushed-to-pushremote
              ((:right-align t)
               (:sort <)))
             ("B" 3 magit-repolist-column-branches
              ((:right-align t)
               (:sort <)))
             ("S" 3 magit-repolist-column-stashes
              ((:right-align t)
               (:sort <))))))

;; Get the link to GitHub/GitLab from a buffer
(use-package git-link
  :config
  (defhydra hydra-git-link (:color blue)
    "Copy git-link"
    ("h" git-link-homepage "Copy homepage")
    ("l" git-link "Copy link")
    ("c" git-link-commit "Copy commit"))
  :bind ("C-c L" . hydra-git-link/body))

(provide 'init-magit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-magit.el ends here
