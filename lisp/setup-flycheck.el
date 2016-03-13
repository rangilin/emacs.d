(require 'use-package)

(use-package flycheck
  :config
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
    (setq flycheck-keymap-prefix (kbd "C-c c-f"))
    (define-key flycheck-mode-map flycheck-keymap-prefix
      flycheck-command-map)

  (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
  (setq flycheck-emacs-lisp-load-path load-path)
  (setq-default flycheck-emacs-lisp-load-path load-path)
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package flycheck-cask
  :config (add-hook 'flycheck-mode-hook 'flycheck-cask-setup))

(provide 'setup-flycheck)
