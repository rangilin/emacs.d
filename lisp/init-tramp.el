(use-package tramp
  :config
  ;; assign tramp file location
  (setq-default tramp-persistency-file-name(expand-file-name "tramp" rangi-generated-files-directory))
  ;; backup remote file in local machine instead of remote server
  (setq tramp-backup-directory-alist backup-directory-alist)


  ;; clean all tramp connections & buffers
  (defun rangi-tramp-cleanup-all ()
    (interactive)
    (tramp-cleanup-all-buffers)
    (tramp-cleanup-all-connections)
    (message "Cleaned up all tramp buffers & connections"))

  (bind-key "C-c t c" 'rangi-tramp-cleanup-all)

  ;; for debugging tramp
  ;; (setq-default tramp-debug-buffer t)
  ;; (setq-default tramp-verbose 10)

  ;; ssh is faster then default scp
  (setq tramp-default-method "ssh"))



(provide 'init-tramp)
