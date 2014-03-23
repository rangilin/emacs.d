(require 'use-package)

(use-package tramp
  :init
  (progn
    (setq-default tramp-persistency-file-name (expand-file-name "tramp" rangi/gen-dir))))

(provide 'setup-tramp)
