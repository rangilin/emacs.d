;; hide toolbar
(tool-bar-mode -1)

;; ido 
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-virtual-buffers t)
(setq ido-max-directory-size 100000)

;; theme
(load-theme 'tango t)

(provide 'init-misc)
