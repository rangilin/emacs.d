(require 'use-package)

(use-package sql
  :init
  (progn
    (use-package sql-indent
      :init
      (progn
        (setq-default sql-indent-offset tab-width)))
    ))

(provide 'setup-sql)
