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
    (set indent-tabs-mode t)))

(provide 'setup-go)
