;;;; Refresh buffer

;; press f5 to refresh buffer
(defun rangi-refresh-buffer ()
  "Referesh current buffer."
  (interactive)
  (revert-buffer nil t nil)
  (message "buffer is refreshed"))

(global-set-key (kbd "<f5>") 'rangi-refresh-buffer)

(provide 'init-buffer)
