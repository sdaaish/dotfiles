;;; INIT-FONTS --- Summary
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

;; Setup an emoji font depending on OS
(cond ((member "Segoe UI Emoji" (font-family-list))
       (set-fontset-font t 'symbol (font-spec :family "Segoe UI Emoji" :fontified t) nil 'prepend)
       (set-fontset-font t 'unicode (font-spec :family "Segoe UI Emoji" :fontified t) nil 'prepend))
      ((member "Noto Color Emoji" (font-family-list))
       (set-fontset-font t 'symbol (font-spec :family "Noto color emoji" :fontified t) nil 'prepend)
       (set-fontset-font t 'unicode (font-spec :family "Noto color emoji" :fontified t) nil 'prepend)))


(provide 'init-fonts)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-fonts.el ends here
