;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((auto-mode-alist . (("\\.toml\\.tmpl\\'" . toml-mode)))
 (auto-mode-alist . (("\\.chezmoi.*" . toml-mode)))
 (auto-mode-alist . (("\\.ps1\\.tmpl\\'" . powershell-mode)))
 (auto-mode-alist . (("\\.sh\\.tmpl\\'" . sh-mode)))
 (conf-toml-mode . ((smartparens-mode . 1)))
 (powershell-mode . ((powershell-indent . 4))))
