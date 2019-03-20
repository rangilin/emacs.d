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
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; always install package if not exist
(setq use-package-always-ensure t)


;; remind me to update packages
(add-hook 'emacs-startup-hook
          (lambda ()
	    (interactive)
	    (when (y-or-n-p "Do you want to check package update ?")
	      (package-list-packages))))



;;
;; Configurations
;; ----------------------------------------------------------------------------
;;


(require 'init-autosave-and-backup)
(require 'init-editor)
(require 'init-gui)
