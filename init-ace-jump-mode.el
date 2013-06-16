(require-package 'ace-jump-mode)

(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back" t)

(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))



(provide 'init-ace-jump-mode)
