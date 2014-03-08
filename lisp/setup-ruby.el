(require 'use-package)

;; ------------------------------ ruby
(use-package ruby-mode
  :mode (("\\.rake$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("Rakefile$" . ruby-mode))
  :init
  (progn
    (use-package rhtml-mode
      :mode (("\\.html\\.erb$" . rhtml-mode)))
    (use-package yari)
    (use-package rspec-mode
      :config
      (progn
        (defvar rspec-use-rvm t)))
    (use-package rinari
      :init (global-rinari-mode 1)
      :config (setq ruby-insert-encoding-magic-comment nil)))
  :config
  (progn
    (bind-key "RET" 'reindent-then-newline-and-indent ruby-mode-map)
    (bind-key "TAB" 'indent-for-tab-command ruby-mode-map)
    (setq ruby-deep-indent-paren nil)
    (custom-set-variables '(ruby-insert-encoding-magic-comment nil))))

;; ------------------------------ rvm
(use-package rvm
  :init (rvm-use-default)
  :config
  (progn
    (add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)))

(provide 'setup-ruby)
