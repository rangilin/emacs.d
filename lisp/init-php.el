(use-package php-mode
  :mode (("\\.php\\'" . php-mode))
  :bind
  (:map php-mode-map
        ("C-c m c c" . php-current-class)
        ("C-c m c n" . php-current-namespace))
  :config

  (use-package company-php
    :config
    (add-to-list 'company-backends 'company-ac-php-backend))

  (defun rangi-set-php-mode ()
    (company-mode t)
    (ac-php-core-eldoc-setup)
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends 'company-ac-php-backend))
  (add-hook 'php-mode-hook 'rangi-set-php-mode))


(use-package psysh)

(provide 'init-php)
