;;; -*- lexical-binding: t; -*-

;; use emacs package manager to manage packages.
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(package-refresh-contents)


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
(require 'module-ivy)
(require 'module-hydra)
(require 'module-org)
(require 'module-programming)
(require 'module-sql)
(require 'module-svc)
(require 'module-web)
(require 'module-terminal)
(require 'module-projectile)


;; initialize modules
(rl-init-module-editor)
(rl-init-module-file)
(rl-init-module-font)
(rl-init-module-gui)
(rl-init-module-ivy)
(rl-init-module-hydra)
(rl-init-module-org)
(rl-init-module-programming)
(rl-init-module-sql)
(rl-init-module-svc)
(rl-init-module-web)
(rl-init-module-terminal)
(rl-init-module-projectile)


;; Put all customizations into custom.el and load the file if it is already exists.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; swap option & command modifier
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))


(setq gc-cons-threshold 100000000)
(setq gnutls-min-prime-bits 4096)
;; Remove disabled commands.
(setq disabled-command-function nil)


;; TODO
;;
;; => daemon / emacsclent integration
