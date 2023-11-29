;;; INIT-UTILS --- Summary
;;
;; Author: Stig Dahl
;; Created: 2023-11-29
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

(use-package copy-as-format
  :config
  (defhydra hydra-copy-as-format (:color blue :columns 3)
    "Copy as format"
    ("a" copy-as-format-asciidoc "asciidoc")
    ("d" copy-as-format-disqus   "disqus")
    ("g" copy-as-format-github   "github/lab/bucket")
    ("H" copy-as-format-hipchat  "hipchat")
    ("h" copy-as-format-html     "html")
    ("j" copy-as-format-jira     "jira")
    ("m" copy-as-format-markdown "markdown")
    ("M" copy-as-format-mediawik "mediawiki")
    ("o" copy-as-format-org-mode "org-mode")
    ("p" copy-as-format-pod      "pod")
    ("r" copy-as-format-rst      "rst")
    ("s" copy-as-format-slack    "slack")
    ("q" nil "quit"))
  :bind ("C-c w" . hydra-copy-as-format/body))

;; For testing packages without installing
(use-package try
  :commands try)

;; HTMLize any file or buffer
(use-package htmlize)

;; File tree navigator
(use-package ztree
  :bind ("C-x z" . ztree-dir)
  :config (setq-default ztree-dir-show-filtered-files t))

(use-package rainbow-delimiters
  :hook
  ((org-mode prog-mode) . rainbow-delimiters-mode))

(use-package rainbow-mode
  :diminish rainbow-mode
  :hook (help-mode . rainbow-mode))

(provide 'init-utils)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
