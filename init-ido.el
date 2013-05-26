(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-virtual-buffers t)
(setq ido-max-directory-size 100000)

(require-package 'smex)
(global-set-key (kbd "M-x") 'smex)

(provide 'init-ido)
