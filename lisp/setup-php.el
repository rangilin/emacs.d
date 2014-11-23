(require 'use-package)

(use-package php-mode
  :config
  (progn
    (setq-default php-mode-coding-style 'drupal)
    (bind-key "C-M-h" 'backward-kill-word php-mode-map)))

(use-package php-boris-minor-mode
  :config
  (progn
    (add-hook 'php-mode-hook 'php-boris-minor-mode)))

(provide 'setup-php)
