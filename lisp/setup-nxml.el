(require 'use-package)

(use-package nxml-mode
  :mode
  (("\\.zul$" . nxml-mode)
   ("\\.xml$" . nxml-mode))
  :init
  (progn
    (setq-default nxml-child-indent tab-width)
    (setq-default nxml-outline-child-indent tab-width)
    (setq-default nxml-slash-auto-complete-flag t)
    (defun rangi--setup-nxml-mode ()
      (modify-syntax-entry ?\" "\""))
    (add-hook 'nxml-mode-hook 'rangi--setup-nxml-mode)))


(use-package sgml-mode
  :config
  (progn
    (bind-key "C-c r" 'mc/mark-sgml-tag-pair html-mode-map)))


(use-package zencoding-mode
  :init
  (progn
    (bind-key "C-j" nil zencoding-mode-keymap)
    (add-hook 'html-mode-hook 'zencoding-mode)
    (add-hook 'nxml-mode-hook 'zencoding-mode)))

(provide 'setup-nxml)
