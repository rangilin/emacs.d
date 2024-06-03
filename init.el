;;; -*- lexical-binding: t -*-

;;
;; Bootstrap
;;----------------------------------------------------------------------------
;;

;; create generated file directory
(unless (file-exists-p rangi-generated-files-directory)
  (make-directory rangi-generated-files-directory))

;; enable all disabled commands
(setq disabled-command-function nil)

;; make emacs save all customizations into 'custom.el'
(setq custom-file (expand-file-name "custom.el" rangi-generated-files-directory))
(when (file-exists-p custom-file) (load custom-file))

;; add files in 'lisp' directory into load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; add all files of sub directories in 'site-listp' into load path
(let ((base (expand-file-name "site-lisp" user-emacs-directory)))
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name) (not (equal f "..")) (not (equal f ".")))
        (add-to-list 'load-path name)))))

;; only display compile log when log level is error
(setq warning-minimum-level :error)

;; inrcease gc threshold before configuration is completed and adjust it back after
(defun rangi-before-config-hook ()
  (eval-and-compile
    (setq gc-cons-threshold 8000000)
    (setq gc-cons-percentage 0.6)))

(defun rangi-after-config-hook ()
  (setq gc-cons-threshold 80000)
  (setq gc-cons-percentage 0.1))

(add-hook 'before-init-hook #'rangi-before-config-hook)
(add-hook 'after-init-hook  #'rangi-after-config-hook)


;;
;; Packages
;; ---------------------------------------------------------------------------
;;

(require 'package)

;; set up archives
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; set up 'use-package' to initialize packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'delight)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; always install package if not exist
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; update package automatically
(use-package auto-package-update
  :config
  (setq auto-package-update-last-update-day-path
        (expand-file-name auto-package-update-last-update-day-filename rangi-generated-files-directory))
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; prefer to load newer version package
(setq load-prefer-newer t)



;;
;; Configurations
;; ----------------------------------------------------------------------------
;;

;; some libraries used in the configurations
(use-package f)
(use-package s)
(use-package dash)
(use-package async)
(use-package hydra)

;; use delight to allow use-package to hide stuff in modeline
(use-package delight)

;; load first to set up environment
(require 'init-env)

(require 'init-accounting)
(require 'init-autosave-and-backup)
(require 'init-browse)
(require 'init-buffer)
(require 'init-company)
(require 'init-copy)
(require 'init-devop)
(require 'init-dired)
(require 'init-docker)
(require 'init-editor)
(require 'init-emacs-lisp)
(require 'init-gui)
(require 'init-macos)
(require 'init-markdown)
(require 'init-misc)
(require 'init-org)
(require 'init-ruby)
(require 'init-php)
(require 'init-shell)
(require 'init-sql)
(require 'init-tramp)
(require 'init-vc)
(require 'init-web)
