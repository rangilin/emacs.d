(require 'use-package)

(use-package js
  :mode (("\\.json$" . js-mode)))

(use-package js2-mode
  :mode (("\\.js$" . js2-mode))
  :config
  (progn
    (setq-default js2-basic-offset 2)
    (setq-default js2-bounce-indent-p t)))

(provide 'setup-js)
