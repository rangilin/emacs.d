;;; -*- lexical-binding: t -*-

;;
;; Bootstrap
;;----------------------------------------------------------------------------
;;

;; create cache directory if not exist
(unless (file-exists-p rangi-emacs-cache-directory)
  (make-directory rangi-emacs-cache-directory))

;; make emacs save all customizations into 'custom.el', also load it if it exist
(setq custom-file (expand-file-name "custom.el" rangi-emacs-cache-directory))
(when (file-exists-p custom-file) (load custom-file))

;; add files in 'lisp' directory into load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; add all files of sub directories in 'site-listp' into load path
(let ((base (expand-file-name "site-lisp" user-emacs-directory)))
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name) (not (equal f "..")) (not (equal f ".")))
        (add-to-list 'load-path name)))))

;; only display compile error log
(setq warning-minimum-log-level :error)
(setq warning-minimum-level :error)

;; use more memory so set a larger threshold
(setq gc-cons-threshold (* 128 1024 1024))

;; improve performance reading from process
(setq read-process-output-max (* 64 1024 1024))
(setq process-adaptive-read-buffering nil)



;;
;; Packages
;; ---------------------------------------------------------------------------
;;

(require 'package)

;; set up archives
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; compile package during installation
(setq package-native-compile t)

;; prefer to load newer version of file if multiple exist
(setq load-prefer-newer t)

;; initialize package
(package-initialize)

;; set up 'use-package' to initialize packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; always install package if not exist
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; update package automatically
(use-package auto-package-update
  :config
  (setq auto-package-update-last-update-day-path
        (expand-file-name auto-package-update-last-update-day-filename rangi-emacs-cache-directory))
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))



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

;; rest of the packages and configurations
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
(require 'init-go)
(require 'init-gui)
(require 'init-macos)
(require 'init-markdown)
(require 'init-misc)
(require 'init-org)
(require 'init-php)
(require 'init-project)
(require 'init-ruby)
(require 'init-shell)
(require 'init-sql)
(require 'init-tramp)
(require 'init-vc)
(require 'init-web)
