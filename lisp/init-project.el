(require-package 'projectile)
(require-package 'counsel-projectile)

(setq-default projectile-mode-line
              '(:eval (format " P[%s]" (projectile-project-name))))

(setq projectile-switch-project-action #'projectile-dired)
(setq projectile-cache-file (expand-file-name "projectile.cache" rangi-generated-files-directory))
(setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" rangi-generated-files-directory))

(counsel-projectile-mode)

(provide 'init-project)
