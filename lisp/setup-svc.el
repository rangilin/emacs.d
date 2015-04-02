(require 'use-package)

(use-package magit
  :init
  (progn

    (setq-default magit-auto-revert-mode-lighter "")
    (setq-default magit-last-seen-setup-instructions "1.4.0")

    (defun rangi/disable-magit-item-highlight ()
      (face-remap-add-relative 'magit-item-highlight '()))
    (add-hook 'magit-status-mode-hook 'rangi/disable-magit-item-highlight)))

(use-package monky
  :config
  (progn
    (setq monky-process-type 'cmdserver)))

(use-package dsvn
  :init
  (progn
     (autoload 'svn-status "dsvn" "Run `svn status'." t)
     (autoload 'svn-update "dsvn" "Run `svn update'." t)))

(provide 'setup-svc)
