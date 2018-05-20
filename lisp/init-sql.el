(defun rangi-sql-set-mysql-sqli-buffer()
  "Set MySQL SQLi buffer for a buffer in SQL mode"
  (interactive)
  (sql-set-product "mysql")
  (sql-set-sqli-buffer))


(provide 'init-sql)
