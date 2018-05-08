(require 'recentf)

(setq recentf-save-file (expand-file-name "recentf" rangi-generated-files-directory))

;; avoid accidentally access remote file during cleanup
(setq recentf-auto-cleanup 'never)

(setq recentf-max-menu-items 25)

;; save recent files every 30 mins instead of on exiting
(run-at-time nil (* 30 60) 'recentf-save-list)

(recentf-mode 1)

(provide 'init-recentf)
