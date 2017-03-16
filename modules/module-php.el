(defun rl/initialize-module-php ()
  "Initialize PHP module."
  (rl--set-up-php-mode))


(defun rl--set-up-php-mode ()
  "Set up PHP mode."
  (use-package php-mode
    :mode "\\.php\\'"
    :ensure t))



(provide 'module-php)
