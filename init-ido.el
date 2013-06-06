(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-virtual-buffers t)
(setq ido-max-directory-size 100000)

;;----------------------------------------------------------------------------
;; Use smex to improve "execute extend command" act like ido-mode
;;----------------------------------------------------------------------------
(require-package 'smex)
(setq smex-prompt-string "M-a ")


(provide 'init-ido)
