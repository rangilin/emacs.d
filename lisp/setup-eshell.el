(require 'functions)
(require 'use-package)

(use-package "eshell"
  :init
  (progn
    (let ((rangi/eshell-dir (rangi/sub-gen-dir "eshell")))
      (setq-default eshell-directory-name rangi/eshell-dir))))

(provide 'setup-eshell)
