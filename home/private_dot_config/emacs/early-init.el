;;; early-init.el --- Summary
;;; Commentary:

;;; Code:

;; Take note of the startup time
(defvar start-time (float-time (current-time)))
(defun my/format-time (time)
  "Displays formatted TIME."
  (format-time-string "%Y-%m-%d %H:%M:%S.%3N" time))

(message "*** %s @ Reading from early-init.el ***" (my/format-time start-time))

(setq package-enable-at-startup nil)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

(provide 'early-init)
;;; early-init.el ends here
