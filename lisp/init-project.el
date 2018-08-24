(require-package 'projectile)
(require 'projectile)

(setq-default projectile-mode-line
              '(:eval (format " P[%s]" (projectile-project-name))))

(setq projectile-switch-project-action #'projectile-dired)
(setq projectile-cache-file (expand-file-name "projectile.cache" rangi-generated-files-directory))
(setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" rangi-generated-files-directory))

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


(require-package 'counsel-projectile)
(counsel-projectile-mode)

(provide 'init-project)
