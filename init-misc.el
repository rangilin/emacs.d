;; hide toolbar
(tool-bar-mode -1)

;; theme
(load-theme 'tango t)

;; git
(require-package 'magit)

;; backup
(setq backup-by-copying t)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))

(provide 'init-misc)
