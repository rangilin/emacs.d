(use-package tramp
  :config

  ;; for debugging tramp
  ;; (setq-default tramp-debug-buffer t)
  ;; (setq-default tramp-verbose 10)

  ;; assign tramp file location
  (setq-default tramp-persistency-file-name(expand-file-name "tramp" rangi-generated-files-directory))

  ;; clean all tramp connections & buffers
  (defun rangi-tramp-cleanup-all ()
    (interactive)
    (tramp-cleanup-all-buffers)
    (tramp-cleanup-all-connections)
    (message "Cleaned up all tramp buffers & connections"))

  (bind-key "C-c t c" 'rangi-tramp-cleanup-all))


(provide 'init-tramp)
