(defun rl-init-module-programming ()
  (rl--set-up-flycheck)
  (rl--set-up-comment)
  (rl--set-up-php))


(defun rl--set-up-flycheck ()
  (use-package flycheck
    :ensure t
    :diminish flycheck-mode
    :config (global-flycheck-mode)))


(defun rl--set-up-comment ()
  "Set up comment behaviors."
  (setq comment-empty-lines t)
  (bind-key "C-/" 'comment-dwim))


(defun rl--set-up-php ()
  "Set up PHP mode."
  (use-package php-mode
    :mode "\\.php\\'"
    :ensure t))



(provide 'module-programming)
