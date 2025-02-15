;;; early-init.el --- Early initialization file -*- lexical-binding: t; no-byte-compile: t -*-

;; prepare a directory to put generated files
(defvar rangi-emacs-cache-directory
  (file-name-as-directory (expand-file-name ".cache" user-emacs-directory))
  "Directory to put Emacs generated temporary files")
(unless (file-exists-p rangi-emacs-cache-directory)
  (make-directory rangi-emacs-cache-directory))

;; add "lisp" directory to load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; add all sub directories in "site-lisp" directory to load-path
(dolist (f (directory-files (expand-file-name "site-lisp" user-emacs-directory) t "^[^\\.]"))
  (when (file-directory-p f)
    (add-to-list 'load-path f)))

;; don't load packages at startup
(setq package-enable-at-startup nil)

;; move natively-compiled stuff to cache directory
(startup-redirect-eln-cache rangi-emacs-cache-directory)

;; prefer to load newer version of file if multiple exist
(setq load-prefer-newer t)


;;; early-init.el ends here
