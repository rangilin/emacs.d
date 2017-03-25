(defun rl-init-module-svc ()
  (rl--set-up-magit))


(defun rl--set-up-magit ()
  (use-package magit
    :ensure t))


(provide 'module-svc)
