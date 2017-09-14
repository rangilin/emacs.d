(defun rl-init-module-sql ()
  (use-package sql-indent :ensure t)

  (setq-default sql-mysql-program "/usr/local/bin/mysql"))



(provide 'module-sql)
