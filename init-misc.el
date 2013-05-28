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
(setq-default indent-line-function 'insert-tab)
(setq-default line-number-mode 1)
(setq-default column-number-mode 1)

(setq-default x-select-enable-clipboard t)

;; hide welcome message
(setq inhibit-splash-screen t)

;; yes/no => y/n
(fset 'yes-or-no-p 'y-or-n-p)




(provide 'init-misc)
