(require 'use-package)

(use-package flycheck
  :config
  (progn
    (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
    (setq flycheck-emacs-lisp-load-path load-path)
    (setq-default flycheck-emacs-lisp-load-path load-path)
    (add-hook 'after-init-hook 'global-flycheck-mode)))

(use-package flycheck-cask
  :config (add-hook 'flycheck-mode-hook 'flycheck-cask-setup))

(provide 'setup-flycheck)
