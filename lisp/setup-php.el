(require 'use-package)

(use-package php-mode
  :mode "\\.php\\'"
  :init
  (progn
    (setq-default php-mode-coding-style 'psr2))
  :config
  (progn
    (defun rangi-setup-php-mode ()
      (add-hook 'before-save-hook 'delete-trailing-whitespace)
      (setq show-trailing-whitespace t))

    (add-hook 'php-mode-psr2-hook 'rangi-setup-php-mode)
    (bind-key "C-M-h" 'backward-kill-word php-mode-map)))
(provide 'setup-php)
