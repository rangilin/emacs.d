(require 'use-package)

(use-package go-mode
  :mode (("\\.go$" . go-mode))
  :config
  (progn
    (defun rangi-go-mode-hook ()
      (setq tab-width 4)
      (setq indent-tabs-mode t))

    (add-hook 'go-mode-hook 'rangi-go-mode-hook)
    (add-hook 'before-save-hook 'gofmt-before-save)

    (defun rangi-godoc (args)
      (interactive "P")
      (if args
          (call-interactively 'godoc)
        (call-interactively 'godoc-at-point)))

    (bind-key "C-c C-r" 'go-remove-unused-imports go-mode-map)
    (bind-key "C-c C-q" 'rangi-godoc go-mode-map)
    (bind-key "C-c i" 'go-goto-imports go-mode-map)
    (bind-key "C-c C-S-j" 'godef-jump-other-window go-mode-map)

    ;; ------------------------------ go-eldoc
    (use-package go-eldoc
      :config
      (progn
        (add-hook 'go-mode-hook 'go-eldoc-setup)))))

(provide 'setup-go)
