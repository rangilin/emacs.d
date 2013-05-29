;; hide toolbar
(tool-bar-mode -1)

;; git
(require-package 'magit)

;; backup
(setq backup-by-copying t)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))

;; editing
(setq-default
 show-trailing-whitespace t
 indent-tabs-mode nil
 tab-width 4
 line-number-mode 1
 column-number-mode 1
 delete-selection-mode t
 x-select-enable-clipboard t
 line-spacing 0.1)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh non-file buffer like dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)



;; hide welcome message
(setq inhibit-splash-screen t)

;; yes/no => y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; rvm
(require-package 'rvm)
(require 'rvm)

(rvm-use-default)

;; yasnippet
(require-package 'yasnippet)
(require 'yasnippet)

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1) ; must after set directory so it can load snippets from there

;; exec path
(require-package 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(provide 'init-misc)
