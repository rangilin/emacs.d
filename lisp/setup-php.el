(require 'use-package)

(use-package php-mode
  :config
  (progn
    (setq-default php-mode-coding-style 'drupal)))

(provide 'setup-php)
