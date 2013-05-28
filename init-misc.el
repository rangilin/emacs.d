;; hide toolbar
(tool-bar-mode -1)

;; git
(require-package 'magit)

;; backup
(setq backup-by-copying t)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))

;; editing
(setq-default show-trailing-whitespace t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; hide welcome message
(setq inhibit-splash-screen t)

;; yes/no => y/n
(fset 'yes-or-no-p 'y-or-n-p)



(provide 'init-misc)
