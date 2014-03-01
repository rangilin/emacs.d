;; -------------------------------------------------- variables
(defvar rl/emacs-dir user-emacs-directory
  "Directory of emacs configuration")

(defvar rl/gen-dir (expand-file-name ".gen" rl/emacs-dir)
  "Directory to put files that generated by emacs which I don't really care")

(defvar rl/startup-file "/ramsey/Dropbox/org/personal/index.org"
  "File to open when emacs is started")

;; -------------------------------------------------- boostrap
(add-to-list 'load-path rl/emacs-dir)
(add-to-list 'load-path (expand-file-name "setups" rl/emacs-dir))

;; create directory if not exist
(make-directory rl/gen-dir t)

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'use-package)
(require 'functions)

;; -------------------------------------------------- setup modules
(require 'setup-backup-n-autosave)
(require 'setup-buffer)
(require 'setup-css)
(require 'setup-editing)
(require 'setup-elisp)
(require 'setup-eshell)
(require 'setup-flycheck)
(require 'setup-flyspell)
(require 'setup-font)
(require 'setup-gui)
(require 'setup-ido)
(require 'setup-js)
(require 'setup-nxml)
(require 'setup-org)
(require 'setup-others)
(require 'setup-recentf)
(require 'setup-ruby)
(require 'setup-search-n-replace)
(require 'setup-sql)
(require 'setup-svc)
(require 'setup-terminal)
(require 'setup-util)
(require 'setup-window)

;;-------------------------------------------------- unbind keys
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-?"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-SPC"))

;; -------------------------------------------------- load customization
(let ((custom-file (expand-file-name "custom.el" user-emacs-directory)))
  (when (file-exists-p custom-file)
    (load custom-file)))

;; -------------------------------------------------- read local configuration
(require 'local nil t)

;; -------------------------------------------------- after
;; annoying!
(kill-buffer "*Compile-Log*")

;; open startup file
(when (file-exists-p rl/startup-file)
    (find-file rl/startup-file))
