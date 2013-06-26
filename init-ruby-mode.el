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

;; (defun install-rspec-snippets ()
;;   (setq yas-snippet-dirs (cons rspec-snippets-dir (yas-snippet-dirs)))
;;   (yas-load-directory rspec-snippets-dir))

(provide 'init-ruby-mode)
