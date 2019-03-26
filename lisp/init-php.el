(use-package php-mode
  :mode (("\\.php\\'" . php-mode))
  :bind
  (:map php-mode-map
        ("C-c m c c" . php-current-class)
        ("C-c m c n" . php-current-namespace)))


(use-package psysh)

(provide 'init-php)
