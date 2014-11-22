(require 'use-package)

(use-package php-mode
  :config
  (progn
    (setq-default php-mode-coding-style 'drupal)))

(use-package php-boris-minor-mode
  :config
  (progn
    (add-hook 'php-mode-hook 'php-boris-minor-mode)))

(provide 'setup-php)
