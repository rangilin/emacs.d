(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-virtual-buffers t)
(setq ido-max-directory-size 100000)

;;----------------------------------------------------------------------------
;; Use smex to improve 'M-x' act like ido-mode
;;----------------------------------------------------------------------------
(require-package 'smex)



(provide 'init-ido)
