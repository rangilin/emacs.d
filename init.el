;; Use emacs package manager to manage packages.
(require 'package)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.org/packages/")))
(package-initialize)
(package-refresh-contents)



;; Put all customizations into custom.el and load the file if it is already exists.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))



;; Add modules directory into load-path so I can load module from it later
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
;; Load package module first because most of other modules depends on it.
(require 'module-package)
(require 'module-gui)
(require 'module-font)
(require 'module-editor)





(setq gc-cons-threshold 100000000)
(setq gnutls-min-prime-bits 4096)
;; Remove disabled commands.
(setq disabled-command-function nil)
