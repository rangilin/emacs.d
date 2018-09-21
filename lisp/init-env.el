;; set up path from shell
(require-package 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; assign locale because en_TW.UTF-8 doesn't works well for some program
(setenv "LANG" "en_US.UTF-8")


(provide 'init-env)
