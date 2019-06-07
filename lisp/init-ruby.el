(use-package inf-ruby
  :config
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

(use-package rvm
  :config
  (rvm-use-default))

(provide 'init-ruby)
