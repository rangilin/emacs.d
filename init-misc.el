;; git
(require-package 'magit)

;; backup
(setq backup-by-copying t)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))


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

;; dired
(put 'dired-find-alternate-file 'disabled nil)

(provide 'init-misc)
