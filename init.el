;;; -*- lexical-binding: t; -*-

;; always show debug message on error
(setq debug-on-error t)


;; add more updated root certification to make Emacs trust Let's Encrypt
(require 'gnutls)
(add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")


;; increase garbage collection threshold during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'after-init-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))



;; Make emacs save all customizations into 'custom.el'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))


;; add our config files into load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; initialize package.el first (package-initialize)
(require 'init-package)
(require 'init-gui)
(when (eq system-type 'darwin)
  (require 'init-mac-os))


;; Load customazations after emacs configurations is loaded
(when (file-exists-p custom-file)
  (load custom-file))

;; start emacs daemon if not already
(require 'server)
(unless (server-running-p)
  (server-start))


;;; Next:
;; - Confirm exit emacs
