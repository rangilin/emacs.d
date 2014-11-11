(require 'use-package)

(use-package vbnet-mode
  :mode ("\\.vb$" . vbnet-mode)
  :load-path "site-lisp/vbnet-mode")

(provide 'setup-vb)
