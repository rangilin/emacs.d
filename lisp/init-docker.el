(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(provide 'init-docker)
