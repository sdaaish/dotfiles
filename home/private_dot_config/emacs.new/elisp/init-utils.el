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

;; Multi colored matching delimiters
(use-package rainbow-delimiters
  :custom-face (rainbow-delimiters-depth-1-face ((t (:foreground "Yellow"))))
  :hook
  ((org-mode prog-mode) . rainbow-delimiters-mode))

;; To highlight colors in text
(use-package rainbow-mode
  :diminish rainbow-mode
  :hook ((prog-mode help-mode) . rainbow-mode))

;; Save automatically
(use-package super-save
  :diminish
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-idle-duration 8)
  (super-save-remote-files nil)
  (super-save-exclude '(".gpg"))
  (auto-save-default nil)
  (auto-save-no-message t)
  (add-to-list 'super-save-triggers 'ace-window)
  (add-to-list 'super-save-triggers 'magit-status)
  (add-to-list 'super-save-hook-triggers 'find-file-hook)
  (add-to-list 'super-save-hook-triggers 'org-after-refile-insert-hook)
  :config
  (super-save-mode))

(use-package macrostep
  :bind ("C-c e" . macrostep-mode))

;; From Steve Yegge's talk on emacs. [[https://youtu.be/6ZWp05OW1c0][Stevey's Tech Talk E41: Emacs Part 2 - Emergency Emacs]]

(defun my/steal-mouse ()
  "Move the mouse to the upper right corner."
  (interactive)
  (and window-system
       (set-mouse-position
        (selected-frame)
        (1- (frame-width)) -1)))

(use-package speed-type
  :custom
  (speed-type-min-chars 150)
  (speed-type-max-chars 350))

(provide 'init-utils)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
