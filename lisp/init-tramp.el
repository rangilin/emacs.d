(require 'tramp)

;; ssh is faster then default scp
(setq tramp-default-method "ssh")

;; assign tramp file location
(setq-default tramp-persistency-file-name(expand-file-name "tramp" rangi-generated-files-directory))

;; for debugging tramp
;; (setq-default tramp-debug-buffer t)
;; (setq-default tramp-verbose 10)

(provide 'init-tramp)
