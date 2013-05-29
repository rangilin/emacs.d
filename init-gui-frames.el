
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)

;;----------------------------------------------------------------------------
;; Display buffer name on frame title
;;----------------------------------------------------------------------------
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))



(provide 'init-gui-frames)
