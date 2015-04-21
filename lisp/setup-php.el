(require 'use-package)

(use-package php-mode
  :mode "\\.php\\|config.php\\'"
  :config
  (progn
    (setq-default php-mode-coding-style 'psr2)
    (bind-key "C-M-h" 'backward-kill-word php-mode-map)))

(use-package php-boris-minor-mode
  :config
  (progn
    (add-hook 'php-mode-hook 'php-boris-minor-mode)))

(provide 'setup-php)
