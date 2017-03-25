(defun rl-init-module-svc ()
  (rl--set-up-magit))


(defun rl--set-up-magit ()
  (use-package magit
    :ensure t
    :config
    (bind-key "C-x g" 'magit-status)))


(provide 'module-svc)
