;;; EARLY-INIT --- Summary
;;
;; Author: Stig Dahl
;; Created: 2023-11-25
;;

;;
;;; Commentary:
;;

;;
;;; Change log:
;;

;;
;;; Code:
(defvar start-time (float-time (current-time)))
(defun my/format-time (time)
  "Displays formatted TIME."
  (format-time-string "%Y-%m-%d %H:%M:%S.%3N" time))

(message "*** %s @ Reading early-init.el" (my/format-time (current-time)))

(setq package-enable-at-startup nil)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)


;; From Doom, shaves off a second from startup.
(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

(provide 'early-init)
;;; early-init.el ends here
