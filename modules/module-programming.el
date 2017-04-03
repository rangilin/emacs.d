(defun rl-init-module-programming ()
  (rl--set-up-flycheck)
  (rl--set-up-php))


(defun rl--set-up-php ()
  "Set up PHP mode."
  (use-package php-mode
    :mode "\\.php\\'"
    :ensure t))


(defun rl--set-up-flycheck ()
  (use-package flycheck
    :ensure t
    :diminish flycheck-mode
    :config (global-flycheck-mode)))



(provide 'module-programming)
