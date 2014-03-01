;; -------------------------------------------------- boostrap
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (expand-file-name "setups" user-emacs-directory))

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'use-package)

;; -------------------------------------------------- setup modules
(require 'setup-buffer)
(require 'setup-css)
(require 'setup-editing)
(require 'setup-elisp)
(require 'setup-flycheck)
(require 'setup-flyspell)
(require 'setup-font)
(require 'setup-gui)
(require 'setup-ido)
(require 'setup-js)
(require 'setup-nxml)
(require 'setup-org)
(require 'setup-others)
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

;; startup file
(defvar rl/startup-file "/ramsey/Dropbox/org/personal/index.org")

;; -------------------------------------------------- read local configuration
(require 'local nil t)

;; -------------------------------------------------- after
;; annoying!
(kill-buffer "*Compile-Log*")

;; open startup file
(when (file-exists-p rl/startup-file)
    (find-file rl/startup-file))
