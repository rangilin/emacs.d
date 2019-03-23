;;;; SQL indentation
(require 'sql-indent)

;; enable indentation mode in SQL mode
(add-hook 'sql-mode-hook (lambda () (sqlind-minor-mode)))

;; customize indentation
(defvar rangi-sql-indentation-offsets-alist
  `((select-clause 0)
    (insert-clause 0)
    (delete-clause 0)
    (update-clause 0)
    ,@sqlind-default-indentation-offsets-alist))

(add-hook 'sqlind-minor-mode-hook
          (lambda ()
            (setq sqlind-indentation-offsets-alist
                  rangi-sql-indentation-offsets-alist)))



;;;; SQL interactive

;;  set mysql SQLi buffer
(defun rangi-sql-set-mysql-sqli-buffer()
  "Set MySQL SQLi buffer for a buffer in SQL mode"
  (interactive)
  (sql-set-product "mysql")
  (sql-set-sqli-buffer))


(provide 'init-sql)
