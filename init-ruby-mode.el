;;-------------------------------------------------- Ruby-mode
; No # -*- coding: utf-8 -*- anymore
(custom-set-variables
 '(ruby-insert-encoding-magic-comment nil))

;;-------------------------------------------------- RVM
(require-package 'rvm)
(require 'rvm)

(rvm-use-default)

;;-------------------------------------------------- Rspec
(require-package 'rspec-mode)
(require 'rspec-mode)

(setq rspec-use-rvm t)



(provide 'init-ruby-mode)
