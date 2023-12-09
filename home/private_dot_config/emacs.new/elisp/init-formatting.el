;;; INIT-FORMATTING --- Summary
;;
;; Author: Stig Dahl
;; Created: 2023-12-06
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

(use-package csv-mode)
(use-package json-mode)
(use-package toml-mode)
(use-package yaml-mode)
(use-package markdown-mode)
(use-package ssh-config-mode)
(use-package ini-mode)
(use-package git-modes
  :config
  (add-to-list 'auto-mode-alist (cons "/.dockerignore\\'" 'gitignore-mode)))

(use-package yara-mode)

(use-package x509-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.cer\\'" . x509-mode))
  (add-to-list 'auto-mode-alist '("\\.crt\\'" . x509-mode))
  (add-to-list 'auto-mode-alist '("\\.crl\\'" . x509-mode))
  (add-to-list 'auto-mode-alist '("\\.csr\\'" . x509-mode))
  (add-to-list 'auto-mode-alist '("\\.pem\\'" . x509-mode))
  (add-to-list 'auto-mode-alist '("\\.key\\'" . x509-mode))
  (defhydra hydra-x509 (:color blue :columns 2)
    "X509 commands"
    ("a" x509-viewasn1 "View ASN1")
    ("c" x509-viewcert "View certificate")
    ("d" x509-viewdh "View DH")
    ("k" x509-viewkey "View key")
    ("r" x509-viewcrl "View CRL")
    ("q" nil "cancel"))
  :bind (:map x509-mode-map
              ("h" . hydra-x509/body)))

(use-package ielm
  :straight (:type built-in)
  :custom (ielm-noisy nil))

(use-package lua-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'auto-mode-alist '("\\.nse$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

(provide 'init-formatting)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-formatting.el ends here
