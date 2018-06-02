;; -*- lexical-binding: t; -*-


(defconst rangi-generated-files-directory
  (file-name-as-directory (expand-file-name "gen" user-emacs-directory))
  "Path of directory where we put file that generated automatically by packages or Emacs itself")




;;----------------------------------------------------------------------------
;; bootstrap
;;----------------------------------------------------------------------------

;; set up directories
(unless (file-exists-p rangi-generated-files-directory) (make-directory rangi-generated-files-directory))

;; add more updated root certification to make Emacs trust Let's Encrypt
(require 'gnutls)
(add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")

;; enable all disabled commands
(setq disabled-command-function nil)




;;----------------------------------------------------------------------------
;; customization
;;----------------------------------------------------------------------------

;; Make emacs save all customizations into 'custom.el'
(setq custom-file (expand-file-name "custom.el" rangi-generated-files-directory))
(when (file-exists-p custom-file) (load custom-file))




;;----------------------------------------------------------------------------
;; load-path
;;----------------------------------------------------------------------------

;; add our config files into load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; add manual installed packages into load path
(let ((base (expand-file-name "site-lisp" user-emacs-directory)))
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))




;;----------------------------------------------------------------------------
;; bootstrap packages
;;----------------------------------------------------------------------------

;; initialize package.el first (package-initialize) so we can install packages
(require 'init-package)


;; install and load these libaries/packages first because they used by other configs
(require-package 'diminish)
(require-package 'dash)
(require-package 'f)
(require-package 's)
(require-package 'hydra)

(require 'dash)
(require 'f)
(require 's)
(require 'misc)


;; load these setups first, because they too used by other configs
(require 'init-exec-path)




;;----------------------------------------------------------------------------
;; set up packages
;;----------------------------------------------------------------------------

(require 'init-ag)
(require 'init-autosave-and-backup)
(require 'init-browse)
(require 'init-buffer)
(require 'init-dired)
(require 'init-editor)
(require 'init-font)
(require 'init-gui)
(require 'init-ivy)
(require 'init-org)
(require 'init-php)
(require 'init-project)
(require 'init-recentf)
(require 'init-shell)
(require 'init-sql)
(require 'init-tramp)
(require 'init-vc)
(require 'init-web)

(when (eq system-type 'darwin)
  (require 'init-mac-os))
