;;; -*- lexical-binding: t; -*-

;; -------------------------------------------------- bootstrapping
;; add all files under '.emacs/lisp/' to the load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; prepare benchmark
(require 'setup-benchmark)

;; load packages installed by Cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'variables)
(require 'functions)

;; preload some local setting before modules
(require 'local-preload nil t)

;; create generate files directory if not exist
(make-directory rangi-gen-dir t)

;; setup locale
(set-locale-environment "zh_TW.utf-8")

;; -------------------------------------------------- setup modules
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(require 'setup-backup-n-autosave)
(require 'setup-browse)
(require 'setup-buffer)
(require 'setup-css)
(require 'setup-copy)
(require 'setup-cursor)
(require 'setup-dired)
(require 'setup-editing)
(require 'setup-elisp)
(require 'setup-eshell)
(require 'setup-flycheck)
(require 'setup-spell)
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
(require 'setup-vb)
(require 'setup-web-mode)
(require 'setup-window)

(when (rangi-osx-p)
  (require 'setup-mac))

;;-------------------------------------------------- unbind keys
(global-unset-key (kbd "C-SPC"))

;; -------------------------------------------------- load customization
(setq custom-file (expand-file-name "custom.el" rangi-gen-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;; -------------------------------------------------- read local configuration
(require 'local nil t)

;; -------------------------------------------------- after
;; add last so displayed message will not be hide by functions in hook
(add-hook 'after-init-hook 'rangi-show-init-time)

;; annoying!
(kill-buffer "*Compile-Log*")
