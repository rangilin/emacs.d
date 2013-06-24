;; git
(require-package 'magit)
(setq magit-status-buffer-switch-function 'switch-to-buffer)

;; backup
(setq backup-by-copying t)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))

;; yes/no => y/n
(fset 'yes-or-no-p 'y-or-n-p)

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

;; isearch
(setq isearch-allow-scroll 1)

;; autopair
(require-package 'autopair)
(autopair-global-mode)

;; flycheck
(require-package 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck '(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers)))

(provide 'init-misc)
