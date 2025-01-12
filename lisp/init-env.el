;; basic info
(setq user-full-name "Rangi Lin")
(setq user-mail-address (s-join "" '("inb" "ox@me.r" "angil" "in.i" "dv.tw")))

;; set up path info from shell environment to emacs
(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

;; assign locale
(setenv "LANG" "en_US.UTF-8")

(provide 'init-env)
