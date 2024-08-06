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

;; Setup custom fonts
(set-face-attribute 'default nil :family "Cascadia Code NF" :foundry "outline" :slant 'normal :weight 'regular :height 140 :width 'normal)
(set-face-attribute 'fixed-pitch nil :family "Cascadia Code" :height 140)
(set-face-attribute 'fixed-pitch-serif nil :family "Cascadia Code NF" :height 140)
(add-to-list 'default-frame-alist '(font . "Cascadia Code"))
(set-frame-font "Cascadia Code NF" nil t)

;; Chose font based on os
(cond ((eq system-type 'gnu/linux)
       (set-face-attribute 'variable-pitch nil :family "ComicShannsMono Nerd Font" :height 140))
      ((eq system-type 'windows-nt)
       (set-face-attribute 'variable-pitch nil :family "Comic Sans MS" :height 140)))

(set-face-attribute 'mode-line-active nil :inherit 'mode-line :foreground "khaki")

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
