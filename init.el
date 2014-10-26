;; -------------------------------------------------- boostrap
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'variables)
(require 'functions)
(require 'use-package)

;; create generate files directory if not exist
(make-directory rangi/gen-dir t)

;; -------------------------------------------------- setup modules
(require 'setup-autopair)
(require 'setup-backup-n-autosave)
(require 'setup-browser)
(require 'setup-buffer)
(require 'setup-css)
(require 'setup-copy)
(require 'setup-cursor)
(require 'setup-dired)
(require 'setup-editing)
(require 'setup-elisp)
(require 'setup-eshell)
(require 'setup-flycheck)
;(require 'setup-spell)
(require 'setup-font)
(require 'setup-go)
(require 'setup-gui)
(require 'setup-guide-key)
(require 'setup-limbo)
(require 'setup-ido)
(require 'setup-js)
(require 'setup-markdown)
(require 'setup-nxml)
(require 'setup-org)
(require 'setup-others)
(require 'setup-php)
(require 'setup-recentf)
(require 'setup-ruby)
(require 'setup-search-n-replace)
(require 'setup-sql)
(require 'setup-svc)
(require 'setup-terminal)
(require 'setup-tramp)
(require 'setup-web-mode)
(require 'setup-window)

;;-------------------------------------------------- unbind keys
(global-unset-key (kbd "C-SPC"))

;; -------------------------------------------------- load customization
(setq custom-file (expand-file-name "custom.el" rangi/gen-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;; -------------------------------------------------- read local configuration
(require 'local nil t)

;; -------------------------------------------------- after
;; annoying!
(kill-buffer "*Compile-Log*")
