;;; INIT-BABEL --- Summary
;;
;; Author: Stig Dahl
;; Created: 2023-11-30
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

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell      . t)
   (eshell     . t)
   (python     . t)
   (lisp       . t)
   (powershell . t)
   (perl       . t)
   (emacs-lisp . t)))



(provide 'init-babel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-babel.el ends here
