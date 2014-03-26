;; -------------------------------------------------- variables
(defvar rangi/emacs-dir user-emacs-directory
  "Directory of emacs configuration")

(defvar rangi/gen-dir (file-name-as-directory (expand-file-name ".gen" rangi/emacs-dir))
  "Directory to put files that generated by Emacs")

(defvar rangi/startup-file "/ramsey/Dropbox/org/personal/index.org"
  "File to open when emacs is started")

;; -------------------------------------------------- boostrap
(add-to-list 'load-path rangi/emacs-dir)
(add-to-list 'load-path (expand-file-name "lisp" rangi/emacs-dir))

;; create directory if not exist
(make-directory rangi/gen-dir t)

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'use-package)
(require 'functions)

;; -------------------------------------------------- setup modules
(require 'setup-autopair)
(require 'setup-backup-n-autosave)
(require 'setup-browser)
(require 'setup-buffer)
(require 'setup-css)
(require 'setup-editing)
(require 'setup-elisp)
(require 'setup-eshell)
(require 'setup-flycheck)
(require 'setup-flyspell)
(require 'setup-font)
(require 'setup-go)
(require 'setup-gui)
(require 'setup-limbo)
(require 'setup-ido)
(require 'setup-js)
(require 'setup-markdown)
(require 'setup-nxml)
(require 'setup-org)
(require 'setup-others)
(require 'setup-recentf)
(require 'setup-ruby)
(require 'setup-search-n-replace)
(require 'setup-sql)
(require 'setup-svc)
(require 'setup-terminal)
(require 'setup-tramp)
(require 'setup-window)

;;-------------------------------------------------- unbind keys
(global-unset-key (kbd "C-SPC"))
(global-unset-key (kbd "M-o"))
(global-unset-key (kbd "M-O"))

;; -------------------------------------------------- load customization
(setq custom-file (expand-file-name "custom.el" rangi/gen-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;; -------------------------------------------------- read local configuration
(require 'local nil t)

;; -------------------------------------------------- after
;; annoying!
(kill-buffer "*Compile-Log*")

;; startup file
(when (file-exists-p rangi/startup-file)
    (find-file rangi/startup-file))

(defun rangi--open-startup-file ()
  (interactive)
  (if (file-exists-p rangi/startup-file)
    (find-file rangi/startup-file)
    (message (format "File %s does not exist" rangi/startup-file))))

(bind-key "C-c <home>" 'rangi--open-startup-file)

(set-face-attribute 'mode-line nil :family "sans" :height 140)
(set-face-attribute 'mode-line-inactive nil :box nil)
(set-face-attribute hl-line-face nil :background "#332233")
