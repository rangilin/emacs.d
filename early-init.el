;;; early-init.el --- Early initialization file -*- lexical-binding: t; no-byte-compile: t -*-

;; prepare a directory to put generated files
(defvar rangi-emacs-cache-directory
  (file-name-as-directory (expand-file-name ".cache" user-emacs-directory))
  "Directory to put Emacs generated temporary files")
(unless (file-exists-p rangi-emacs-cache-directory)
  (make-directory rangi-emacs-cache-directory))

;; don't load packages at startup
(setq package-enable-at-startup nil)

;; move natively-compiled stuff to cache directory
(if (native-comp-available-p)
    (startup-redirect-eln-cache rangi-emacs-cache-directory)
  (warn "native compile is not available"))
