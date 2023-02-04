;;; my-cheats.el --- Summary

;;; Commentary:
;; Cheatsheet for Emacs and commands

;;; Code:
(cheatsheet-add-group 'Common
                      '(:key "C-x C-c" :description "leave Emacs")
                      '(:key "C-x C-f" :description "find file")
                      '(:key "C-x t" :description "Treemacs")
                      '(:key "C-x z" :description "ztree-dir")
                      '(:key "C-c s" :description "Show cheatsheet")
                      '(:key "C-h B" :description "Show my keybindings")
                      '(:key "C-;" :description "Resize-Window"))

(cheatsheet-add-group 'Ivy
                      '(:key "C-M-j" :description "Enable new name in buffer")
                      '(:key "C-c C-S-F" :description "counsel-recentf")
                      '(:key "C-s C-S-R" :description "ivy-resume")
                      '(:key "C-c g" :description "Counsel-git")
                      '(:key "C-c j" :description "Counsel-git-grep"))

(cheatsheet-add-group 'IDO
                      '(:key "C-x b" :description "Switch buffers")
                      '(:key "C-f" :description "find file within IDO")
                      '(:key "C-SPC" :description "Limit list to filter")
                      '(:key "C-h f ido-find-file RET" :description "Find ido-documentation"))

(cheatsheet-add-group 'Org-mode
                      '(:key "C-c a t" :description "Start agenda Todo")
                      '(:key "C-c C-e" :description "export org-mode")
                      '(:key "C-c C-o" :description "Open link")
                      '(:key "C-x C-e" :description "eval-last-sexp")
                      '(:key "C-c C-w" :description "Org-refile")
                      '(:key "C-c c" :description "Org-capture")
                      '(:key "C-c C-d" :description "Org-deadline")
                      '(:key "C-c C-s" :description "Org-schedule")
                      '(:key "C-c C-x c" :description "org-clone-subtree-with-time-shift")
                      '(:key "<f8> o o" :description "Switch Org buffers"))

(cheatsheet-add-group 'Hydras
                      '(:key "<f8> i" :description "Emacs config files")
                      '(:key "<f8> o" :description "Org-mode stuff")
                      '(:key "C-c b" :description "Launch shells and software")
                      '(:key "C-c v" :description "Hydra toggle functions")
                      '(:key "C-c o" :description "Org-inserts"))

(cheatsheet-add-group 'Company
                      '(:key "M-n" :description "Select next")
                      '(:key "M-p" :description "Select previous"))

(cheatsheet-add-group 'LISP
                      '(:key "C-x ESC ESC" :description "Repeat last command"))

(cheatsheet-add-group 'Emacs
                      '(:key "prog > /dev/clip" :description "Redirect to clipboard")
                      '(:key "prog > /dev/kill" :description "Redirect to kill ring")
                      '(:key "prog > #<buffer-name>" :description "redirect to buffer-name"))

(cheatsheet-add-group 'Magit
                      '(:key "C-p" :description "magit-git-flow")
                      '(:key "C-c p" :description "magit-find-file-completing-read"))

(cheatsheet-add-group 'Hackernews
                      '(:key "RET" :description "Open post in browser")
                      '(:key "t" :description "Open post in textmode")
                      '(:key "n" :description "Next post")
                      '(:key "p" :description "Previous post")
                      '(:key "tab/S-tab" :description "Next/Previous comment")
                      '(:key "m" :description "load more posts")
                      '(:key "q" :description "quit"))

(cheatsheet-add-group 'avy
                      '(:key "C-." :description "Goto char")
                      '(:key "C-:" :description "Goto char 2"))

(provide 'my-cheats)
;;; my-cheats.el ends here
