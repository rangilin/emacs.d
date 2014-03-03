(require 'use-package)

(use-package nxml-mode
  :mode
  (("\\.zul$" . nxml-mode)
   ("\\.xml$" . nxml-mode))
  :config
  (progn
    (bind-key "M-h" nil nxml-mode-map)
    (setq-default nxml-child-indent tab-width)
    (setq-default nxml-outline-child-indent tab-width)
    (setq nxml-slash-auto-complete-flag t)))

;; ------------------------------ zen coding
(use-package zencoding-mode
  :init
  (progn
    (bind-key "C-j" nil zencoding-mode-keymap)
    (add-hook 'html-mode-hook 'zencoding-mode)
    (add-hook 'nxml-mode-hook 'zencoding-mode)))

(provide 'setup-nxml)
