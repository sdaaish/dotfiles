;; Optional: load other packages before eglot to enable eglot integrations.

(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))

(use-package eglot
  :straight (:type built-in)
  :init (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  :custom (eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (eglot-report-progress nil)

  :config (setq-default eglot-workspace-configuration
                        '(
                          :jedi-language-server (
                                                 :jediSettings (
                                                                :autoImportModules ["numpy" "pandas"]))
                          :pylsp (
                                  :configurationSources ["flake8"]
                                  :plugins (
                                            :jedi_completion (:include_params t
                                                                              :fuzzy t)
                                            :pylint (:enabled :json-false)
                                            :pyflakes (:enabled :json-false)
                                            :flake8 (:enabled :json-false
                                                              :maxLineLength 88)
                                            :ruff (:enabled t
                                                            :lineLength 88)
                                            :pydocstyle (:enabled t
                                                                  :convention "numpy")
                                            :yapf (:enabled :json-false)
                                            :autopep8 (:enabled :json-false)
                                            :black (:enabled t
                                                             :line_length 88
                                                             :cache_config t)))
                          :gopls (
                                  :usePlaceholders t
                                  :staticcheck t
                                  :matcher "CaseSensitive")))

  :hook
  (python-ts-mode . superword-mode)
  (python-ts-mode . hs-minor-mode)
  (python-ts-mode . (lambda () (set-fill-column 88)))
  ((go-ts-mode python-ts-mode) . eglot-ensure)
  ((go-ts-mode python-ts-mode) . eglot-format-buffer-on-save)
  ((go-ts-mode python-ts-mode) . eglot-format-buffer-on-save))


(use-package flycheck)

(provide 'init-eglot)
