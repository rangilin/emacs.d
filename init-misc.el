;; hide toolbar
(tool-bar-mode -1)

;; git
(require-package 'magit)

;; backup
(setq backup-by-copying t)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))

;; editing
(setq-default 
 show-trailing-whitespace t
 indent-tabs-mode nil)

;; customize
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; hide welcome message
(setq inhibit-splash-screen t)

(provide 'init-misc)
