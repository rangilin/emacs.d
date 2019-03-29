(use-package sql
  :config
  ;;  set mysql SQL interactive buffer
  (defun rangi-set-mysql-sqli-buffer()
    "Set MySQL SQLi buffer for a buffer in SQL mode"
    (interactive)
    (sql-set-product "mysql")
    (sql-set-sqli-buffer)))


(use-package sql-indent
  :delight
  :config
  (defvar my-sql-indentation-offsets-alist
    `((select-clause 0)
      (insert-clause 0)
      (delete-clause 0)
      (update-clause 0)
      ,@sqlind-default-indentation-offsets-alist))

  (add-hook 'sqlind-minor-mode-hook
            (lambda ()
              (setq sqlind-indentation-offsets-alist
                    my-sql-indentation-offsets-alist)))

  (add-hook 'sql-mode-hook 'sqlind-minor-mode))


(provide 'init-sql)
