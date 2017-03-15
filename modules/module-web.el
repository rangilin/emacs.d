(defun rl/initialize-module-web ()
    (rl--set-up-restclient))


(defun rl--set-up-restclient ()
    "Set up REST client package."
    (use-package restclient
      :ensure t
      :mode ("\\.rest\\'" . restclient-mode)))



(provide 'module-web)
