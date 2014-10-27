(require 'use-package)

(use-package php-mode
  :init
  (progn
    (bind-key "C-M-h" 'backward-kill-word php-mode-map)))

(provide 'setup-php)
