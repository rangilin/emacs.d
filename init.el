;;; -*- lexical-binding: t; -*-

;; use emacs package manager to manage packages.
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
;(package-refresh-contents)


;; use use-package to install and lazy loading packages
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))


;; load modules
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'module-editor)
(require 'module-file)
(require 'module-font)
(require 'module-gui)
(require 'module-web)


;; initialize modules
(rl/initialize-module-editor)
(rl/initialize-module-file)
(rl/initialize-module-font)
(rl/initialize-module-gui)
(rl/initialize-module-web)


;; Put all customizations into custom.el and load the file if it is already exists.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))



(setq gc-cons-threshold 100000000)
(setq gnutls-min-prime-bits 4096)
;; Remove disabled commands.
(setq disabled-command-function nil)


;; TODO
;;
;; => daemon / emacsclent integration
;;
;;
