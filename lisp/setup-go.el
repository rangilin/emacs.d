(require 'use-package)

(use-package go-mode
  :mode (("\\.go$" . go-mode))
  :init
  (progn
    (use-package go-eldoc
      :config
      (progn
        (add-hook 'go-mode-hook 'go-eldoc-setup))))
  :config
  (progn
    (add-hook 'before-save-hook 'gofmt-before-save)
    (setq indent-tabs-mode t)))

(provide 'setup-go)
