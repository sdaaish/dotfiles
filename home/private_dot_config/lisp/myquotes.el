;;; MYQUOTES --- Summary  -*- lexical-binding:t -*-
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

(defun my/quote-word (arg)
  "Moves forward a word and puts in doublequotes"
  (interactive "p")
  (forward-whitespace 1)
  (insert "\"")
  (forward-word arg)
  (insert "\""))

;; From Emacs Elements
(defun replace-smart-quotes (beg end)
  "Replace 'smart quotes' in buffer or region with ascii quotes.
      Operates on the entire buffer if no region is selected."
  (interactive
   (if (use-region-p)
	     (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (format-replace-strings '(("\x201C" . "\"")
			                      ("\x201D" . "\"")
			                      ("\x2018" . "'")
			                      ("\x2019" . "'"))
			                    nil beg end))
(global-set-key (kbd "C-c q") 'replace-smart-quotes)

;; From Emacs Elements
(defun yank-and-replace-smart-quotes ()
  "Yank (paste) and replace smart quotes from the source with ascii quotes."
  (interactive)
  (yank)
  (replace-smart-quotes (mark) (point)))

;; (global-set-key (kbd "C-c y") 'yank-and-replace-smart-quotes)

(provide 'myquotes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; quoting.el ends here
