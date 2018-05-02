(require 'tramp)

;; ssh is faster then default scp
(setq tramp-default-method "ssh")

;; assign tramp file location
(setq-default tramp-persistency-file-name(expand-file-name "tramp" rangi-generated-files-directory))

;; backup remote file in local machine instead of remote server
(setq tramp-backup-directory-alist backup-directory-alist)

;; for debugging tramp
;; (setq-default tramp-debug-buffer t)
;; (setq-default tramp-verbose 10)

(provide 'init-tramp)
