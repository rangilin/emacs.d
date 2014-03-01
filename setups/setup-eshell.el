(require 'functions)
(require 'use-package)

(use-package "eshell"
  :init
  (progn
    (let ((rl/eshell-dir (rl/sub-gen-dir "eshell")))
      (setq-default eshell-directory-name rl/eshell-dir))))

(provide 'setup-eshell)
