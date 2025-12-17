;;; CODE --- Summary  -*- lexical-binding: t; -*-
;;
;; Author: Stig Dahl
;; Created: 2024-04-16
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


(defun my/prog-mode ()
  "Enables useful setting for programming mode."
  (interactive)
  (smartparens-mode t)
  (rainbow-delimiters-mode t)
  (aggressive-indent-mode t))


;; Random password generator
(require 'cl-lib)

(defvar generate-password-char-set
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()-_=+"
  "Characters used to build passwords.")

(defun generate-random-password (length &optional chars)
  "Generate and return a random password of integer LENGTH.
Optional CHARS is a string of allowed characters (defaults to
=generate-password-char-set').

If called interactively the generated password is copied to the
kill ring and a short confirmation is shown (the password itself
is not printed to the minibuffer)."
  (interactive "nPassword length: ")
  (let* ((chars (or chars generate-password-char-set))
         (ccount (length chars)))
    (unless (and (integerp length) (> length 0))
      (user-error "Length must be a positive integer"))
    (let ((pwd (apply #'string
                      (cl-loop repeat length
                               collect (aref chars (random ccount))))))
      (when (called-interactively-p 'interactive)
        (kill-new pwd)
        (message "Generated password (length %d) copied to kill-ring" length))
      pwd)))

(provide 'code)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; code.el ends here
