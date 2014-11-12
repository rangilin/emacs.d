(require 'use-package)

(use-package vbnet-mode
  :mode ("\\.vb$" . vbnet-mode)
  :load-path "site-lisp/vbnet-mode"
  :config
  (progn
    (defun rangi-setup-vbnet-mode ()
      (toggle-truncate-lines))

    (add-hook 'vbnet-mode-hook 'rangi-setup-vbnet-mode)))

(provide 'setup-vb)
