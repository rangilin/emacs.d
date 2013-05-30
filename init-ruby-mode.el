;;----------------------------------------------------------------------------
;; Ruby-mode key
;;----------------------------------------------------------------------------

(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     (define-key ruby-mode-map (kbd "TAB") 'indent-for-tab-command)))

;;----------------------------------------------------------------------------
;; No # -*- coding: utf-8 -*- anymore
;;----------------------------------------------------------------------------
(custom-set-variables
 '(ruby-insert-encoding-magic-comment nil))

;;----------------------------------------------------------------------------
;; RVM
;;----------------------------------------------------------------------------
(require-package 'rvm)
(require 'rvm)

(rvm-use-default)


(provide 'init-ruby-mode)
