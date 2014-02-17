(require 'use-package)

(use-package magit
  :init
  (progn
    (defun rl/disable-magit-item-highlight ()
      (face-remap-add-relative 'magit-item-highlight '()))
    (add-hook 'magit-status-mode-hook 'rl/disable-magit-item-highlight)))

(use-package monky
  :config
  (progn
    (setq monky-process-type 'cmdserver)))

(provide 'setup-svc)
