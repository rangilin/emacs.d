
(use-package project
  :bind-keymap (("s-p" . project-prefix-map) ; add other keys to access project key map
                ("C-c p" . project-prefix-map))
  :config
  (setq project-list-file (expand-file-name "projects" rangi-emacs-cache-directory)))



(provide 'init-project)
