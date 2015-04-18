(require 'use-package)

(use-package css-mode
  :mode ("\\.css$" . css-mode)
  :config
  (setq-default css-indent-offset tab-width))

(provide 'setup-css)
