;; set up path info from shell environment to emacs
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; assign locale because en_TW.UTF-8 doesn't works well for some program in shell
(setenv "LANG" "en_US.UTF-8")

(provide 'init-env)
