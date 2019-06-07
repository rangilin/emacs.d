(use-package enh-ruby-mode
  :mode (("\\.rb$" . enh-ruby-mode))
  :config
  (setq enh-ruby-deep-indent-paren nil))

(use-package inf-ruby
  :config
  (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode))

(use-package rvm
  :config
  (rvm-use-default))

(provide 'init-ruby)
