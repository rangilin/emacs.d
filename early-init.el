;;; early-init.el --- Early initialization file -*- lexical-binding: t -*-

;; set Emacs user directory to the directory where this file is in
;; this make it easier to organize generated files when opening different
;; Emacs instances
(setq user-emacs-directory default-directory)

;; this is the directory to put generated files
(setq rangi-emacs-cache-directory
      (file-name-as-directory (expand-file-name ".cache" user-emacs-directory)))
(unless (file-exists-p rangi-emacs-cache-directory)
  (make-directory rangi-emacs-cache-directory))

;; don't load packages at startup
(setq package-enable-at-startup nil)

;; move natively-compiled stuff to cache directory
(startup-redirect-eln-cache rangi-emacs-cache-directory)

;;; early-init.el ends here
