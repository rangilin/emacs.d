;;; -*- lexical-binding: t -*-


(defconst rangi-generated-files-directory
  (file-name-as-directory (expand-file-name "gen" user-emacs-directory))
  "Path of directory where we put file that generated automatically by packages or Emacs itself")



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


;; inrcease gc threshold before configuration to decrease time
;; redunce after to reduce GC pause
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

;; install into separate directories for each Emacs version, to prevent bytecode incompatibility
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version) rangi-generated-files-directory)))
  (setq package-user-dir versioned-package-dir))

;; set up archives
(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; set up 'use-package' to install packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'delight)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))


;; prefer to load newer version package
(setq load-prefer-newer t)

;; always install package if not exist
(setq use-package-always-ensure t)



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

(require 'init-autosave-and-backup)
(require 'init-browse)
(require 'init-buffer)
(require 'init-copy)
(require 'init-dired)
(require 'init-editor)
(require 'init-emacs-lisp)
(require 'init-gui)
(require 'init-macos)
(require 'init-markdown)
(require 'init-misc)
(require 'init-org)
(require 'init-shell)
(require 'init-vc)
