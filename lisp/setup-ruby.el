(require 'use-package)

(use-package ruby-mode
  :mode (("\\.rake$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode))
  :init
  (progn
    (setq-default ruby-insert-encoding-magic-comment nil)
    (setq-default ruby-deep-indent-paren nil)

    (defun rangi--setup-ruby-mode ()
      (bind-key "C-c q" 'yari ruby-mode-map))

    (add-hook 'ruby-mode-hook 'rangi--setup-ruby-mode)

    (use-package ruby-electric
      :init
      (progn
        (add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))

        ;; workaround ruby-insert-end void bug
        (defun ruby-insert-end ()
          "Insert \"end\" at point and reindent current line."
          (interactive)
          (insert "end")
          (ruby-indent-line t)
          (end-of-line))))

    (use-package rhtml-mode
      :mode (("\\.html\\.erb$" . rhtml-mode)))

    (use-package yari
      :init (defalias 'ri 'yari))

    (use-package rspec-mode
      :init
      (progn
        (setq-default rspec-use-rvm t)
        (setq-default rspec-use-rake-when-possible nil)))



    (use-package rinari
      :init (global-rinari-mode 1))
  ))

;; ------------------------------ rvm
(use-package rvm
  :init (rvm-use-default)
  :config
  (progn
    (add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)))

(provide 'setup-ruby)
