(defun rl-init-module-projectile ()
  (rl--set-up-projectile))


(defun rl--set-up-projectile ()

  ;; set up projectile
  (use-package projectile
    :ensure t
    :config
    (setq projectile-cache-file
          (expand-file-name "projectile.cache" rl-dir-autogen))
    (setq projectile-enable-caching t)

    (setq projectile-switch-project-action 'projectile-dired)
    (setq projectile-known-projects-file
          (expand-file-name "projectile-bookmarks.eld" rl-dir-autogen))
    (projectile-load-known-projects)
    (projectile-global-mode))

  ;; integrate with counsel
  (use-package counsel-projectile
    :ensure t
    :config
    (counsel-projectile-on)))


(provide 'module-projectile)
