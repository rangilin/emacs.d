(defun rl-init-module-programming ()
  (rl--set-up-flycheck)
  (rl--set-up-auto-complete)
  (rl--set-up-comment)
  (rl--set-up-php)
  (rl--set-up-markdown))


(defun rl--set-up-auto-complete ()
  (use-package company
    :diminish company-mode
    :ensure t))


(defun rl--set-up-flycheck ()
  (use-package flycheck
    :ensure t
    :diminish flycheck-mode
    :init
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
    :config (global-flycheck-mode)))


(defun rl--set-up-comment ()
  "Set up comment behaviors."
  (setq comment-empty-lines t)
  (bind-key "C-/" 'comment-dwim))


(defun rl--set-up-php ()
  "Set up PHP mode."
  (use-package php-mode
    :mode "\\.php\\'"
    :ensure t
    :config
    (add-hook 'php-mode-hook 'rl--php-mode-hook))
  (use-package ac-php :ensure t))


(defun rl--php-mode-hook ()
  "My PHP mode hook."
  (company-mode t)
  (ac-php-core-eldoc-setup)
  (add-to-list 'company-backends 'company-ac-php-backend))


(defun rl--set-up-eldoc ()
  "Set up eldoc mode."
  (use-package eldoc
    :diminish eldoc-mode))


(defun rl--set-up-markdown ()
  "Set up markdown mode"
  (use-package markdown-mode
    :ensure t
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown")))





(provide 'module-programming)
