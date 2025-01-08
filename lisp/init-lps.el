(use-package eglot
  :bind (:map eglot-mode-map
	            ("C-c l h" . eldoc)
	            ("C-c l f" . eglot-format)
	            ("C-c l r" . eglot-rename))
  :config
  (setq eglot-autoshutdown t))


(provide 'init-lps)
