(require 'use-package)

(use-package go-mode
  :mode (("\\.go$" . go-mode))
  :init
  (progn

	(defun rangi/go-mode-hook ()
	  (setq tab-width 4))
	(add-hook 'go-mode-hook 'rangi/go-mode-hook)

	;; go-eldoc
    (use-package go-eldoc
      :config
      (progn
        (add-hook 'go-mode-hook 'go-eldoc-setup))))
  :config
  (progn
    (add-hook 'before-save-hook 'gofmt-before-save)
    (setq tab-width 4)
    (setq indent-tabs-mode t)))

(provide 'setup-go)
