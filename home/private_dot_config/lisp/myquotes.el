;;; myquotes.el --- basic funtions -*- lexical-binding:t -*-

;;; Code:

(defun my/quote-word (arg)
  "Moves forward a word and puts in doublequotes"
  (interactive "p")
  (forward-whitespace 1)
  (insert "\"")
  (forward-word arg)
  (insert "\"")
  )
