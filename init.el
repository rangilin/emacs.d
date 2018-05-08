;;; -*- lexical-binding: t; -*-

;; always show debug message on error
(setq debug-on-error t)


;; add more updated root certification to make Emacs trust Let's Encrypt
(require 'gnutls)
(add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")


;; Set up directory for auto-generated files
(defconst rangi-generated-files-directory
  (file-name-as-directory (expand-file-name "gen" user-emacs-directory))
  "Path of directory where we put file that generated automatically by packages or Emacs itself")
(unless (file-exists-p rangi-generated-files-directory) (make-directory rangi-generated-files-directory))


;; Make emacs save all customizations into 'custom.el'
(setq custom-file (expand-file-name "custom.el" rangi-generated-files-directory))

;; enable all disabled commands
(setq disabled-command-function nil)

;; add our config files into load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


;; initialize package.el first (package-initialize) so we can install packages
(require 'init-package)

;; then get environment variables from shell into Emacs, so we can handle it properly
(require 'init-exec-path)

;; load misc
(require 'misc)

;; load these packages first because they used by other configs
(require-package 'diminish)

;; then initalize rest of the packages
(require 'init-autosave-and-backup)
(require 'init-buffer)
(require 'init-dired)
(require 'init-editor)
(require 'init-font)
(require 'init-gui)
(require 'init-ivy)
(require 'init-org)
(require 'init-recentf)
(require 'init-shell)
(require 'init-tramp)
(require 'init-vc)

(when (eq system-type 'darwin)
  (require 'init-mac-os))


;; Load customazations after packages is initalized
(when (file-exists-p custom-file)
  (load custom-file))


;; start emacs daemon if not already
(require 'server)
(unless (server-running-p)
  (server-start))


;; do stuff after emacs is started
(defun rangi-emacs-startup ()
  ;; close compile log window automatically
  (let ((log "*Compile-Log*"))
    (when (get-buffer log)
      (delete-windows-on "*Compile-Log*"))))

(add-hook 'emacs-startup-hook 'rangi-emacs-startup)
