(require 'use-package)

(use-package js
  :mode (("\\.json$" . js-mode))
  :config
  (progn
    (setq-default js-indent-level tab-width)))

(use-package js2-mode
  :mode (("\\.js$" . js2-mode))
  :config
  (progn
    (setq-default js2-basic-offset tab-width)
    (setq-default js2-bounce-indent-p t)))

(provide 'setup-js)
