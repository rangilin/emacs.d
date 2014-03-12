(require 'use-package)

(use-package nxml-mode
  :mode
  (("\\.zul$" . nxml-mode)
   ("\\.xml$" . nxml-mode))
  :config
  (progn
    (bind-key "C-c r" 'mc/mark-sgml-tag-pair nxml-mode-map)
    (setq-default nxml-child-indent tab-width)
    (setq-default nxml-outline-child-indent tab-width)
    (setq nxml-slash-auto-complete-flag t)))

(use-package sgml-mode
  :config
  (progn
    (bind-key "C-c r" 'mc/mark-sgml-tag-pair html-mode-map)))

;; ------------------------------ zen coding
(use-package zencoding-mode
  :init
  (progn
    (bind-key "C-j" nil zencoding-mode-keymap)
    (add-hook 'html-mode-hook 'zencoding-mode)
    (add-hook 'nxml-mode-hook 'zencoding-mode)))

(provide 'setup-nxml)
