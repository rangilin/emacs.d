(require 'use-package)

(use-package css-mode
  :mode ("\\.css$" . css-mode)
  :init
  (progn
    (setq-default css-indent-offset tab-width)))

(provide 'setup-css)
