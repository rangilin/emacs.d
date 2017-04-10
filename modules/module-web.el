;;; -*- lexical-binding: t; -*-

(defun rl-init-module-web ()
  (rl--set-up-restclient)
  (rl--set-up-webmode))


(defun rl--set-up-restclient ()
    "Set up REST client package."
    (use-package restclient
      :ensure t
      :mode ("\\.rest\\'" . restclient-mode)))


(defun rl--set-up-webmode ()
  "Set up webmode package"
  (use-package web-mode
    :ensure t
    :mode (("\\.html\\'" . web-mode))))


(provide 'module-web)
