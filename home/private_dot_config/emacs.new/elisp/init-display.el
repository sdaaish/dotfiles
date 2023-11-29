;;; INIT-DISPLAY --- Summary
;;
;; Author:  Stig Dahl
;; Created: 27 November 2023-11-27
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

(mouse-avoidance-mode "banish")

(use-package golden-ratio
  :diminish
  :custom
  (golden-ratio-recenter t)
  (golden-ratio-auto-scale t)
  :config
  (golden-ratio-mode 1))

(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-background nil)
  (aw-dispatch-always t)
  :bind (("M-o" . ace-window)
         ("C-x o" . aw-flip-window))
  :custom-face (aw-leading-char-face
                ((t (:foreground "light sky blue"
                                 :background "#434343"
                                 :weight bold :height 2.0)))))

;; Need to disable golden-ratio for this to work. TBD
(use-package resize-window
  :bind ("C-;" . resize-window))

(use-package avy
  :bind (("C-." . avy-goto-char)
         ("C-:" . avy-goto-char-2)))


;; Manage buffers
(use-package ibuffer
  :straight (:type built-in)
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("Dired" (mode . dired-mode))
                 ("Shell" (or
                           (mode . eshell-mode)
                           (mode . shell-mode)))
                 ("PowerShell" (mode . powershell-mode))
                 ("code" (mode . prog-mode))
                 ("Magit" (or
                           (name . "^magit")
                           (name . "\\*magithub.*")))
                 ("Emacs" (or
                           (name . "^\\*scratch\\*$")
                           (name . "^\\*Messages\\*$")
                           (mode . emacs-lisp-mode)))
                 ("Errors" (or
                            (name . ".*error*")
                            (name . ".*[wW]arning")
                            (mode . special-mode)))
                 ("Tramp" (or (filename . "^\\/scp:")
                              (name . "^\\*tramp")))
                 ("iBuffer" (mode . ibuffer-mode))
                 ("Gists" (name . "^\\*gist.*"))
                 ("Org" (or
                         (mode . org-mode)
                         (name . "^\\*Org Agenda\\*$")))
                 ("Help" (or (name . "\\*Help\\*")
                             (name . "\\*Apropos\\*")
                             (name . "\\*info\\*")
                             (mode . help-mode)
                             (mode . helpful-mode))))
                ("Help"
                 ("Man" (mode . man-mode))
                 ("Help" (or (name . "\\*Help\\*")
                             (name . "\\*Apropos\\*")
                             (name . "\\*info\\*")
                             (mode . help-mode)
                             (mode . helpful-mode)))
                 ("Magit" (name . "^magit"))
                 ("dired" (mode . dired-mode)))
                ("Org"
                 ("Org" (or
                         (mode . org-mode)
                         (name . "^\\*Org Agenda\\*$")))))))
  (customize-set-variable 'ibuffer-show-empty-filter-groups nil)
  (customize-set-variable 'ibuffer-expert t)
  (customize-set-variable 'ibuffer-use-other-window t)
  :hook (ibuffer-mode .
                      (lambda ()
                        (ibuffer-auto-mode 1)
                        (ibuffer-switch-to-saved-filter-groups "default")
                        (unless (eq ibuffer-sorting-mode 'alphabetic)
                          (ibuffer-do-sort-by-alphabetic)))))

(use-package ibuffer-vc
  :config
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process)))
  :hook (ibuffer-mode .
                      (lambda ()
                        (ibuffer-vc-set-filter-groups-by-vc-root)
                        (unless (eq ibuffer-sorting-mode 'alphabetic)
                          (ibuffer-do-sort-by-alphabetic)))))

(use-package ibuffer-projectile
  :bind (:map ibuffer-mode-map
              ("c" . ibuffer-projectile-set-filter-groups)
              ("/ -" . ibuffer-filter-by-directory)))

(use-package all-the-icons)

(provide 'init-display)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-display.el ends here
