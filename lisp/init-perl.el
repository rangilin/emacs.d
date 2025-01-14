(use-package cperl-mode
  :mode "\\.pl\\'"
  :interpreter "perl"
  :config
  (setq cperl-invalid-face nil)
  :hook ((cperl-mode . eglot-ensure)))

(provide 'init-perl)
