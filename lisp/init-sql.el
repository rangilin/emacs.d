(use-package sql
  :config
  ;;  set mysql SQL interactive buffer
  (defun rangi-set-mysql-sqli-buffer()
    "Set MySQL SQLi buffer for a buffer in SQL mode"
    (interactive)
    (sql-set-product "mysql")
    (sql-set-sqli-buffer))
  (add-hook 'sql-mode-hook #'rangi-set-mysql-sqli-buffer))


(use-package sql-indent
  :delight
  :config
  (add-hook 'sql-mode-hook 'sqlind-minor-mode))


(provide 'init-sql)
