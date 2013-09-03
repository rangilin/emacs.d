
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

;; so we can make emacsclient maximized
;; http://stackoverflow.com/questions/8363808/
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(provide 'init-gui-frames)
