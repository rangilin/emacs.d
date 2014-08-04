(require 'use-package)

(use-package php-mode
  :init
  (progn
	(defun rangi/php-mode-hook ()
	  (setq indent-tabs-mode t))
	(add-hook 'php-mode-hook 'rangi/php-mode-hook)

    (bind-key "C-M-h" 'rangi/backward-delete-word php-mode-map)))

(provide 'setup-php)
