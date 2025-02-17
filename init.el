;;; init.el --- Emacs initialization file -*- lexical-binding: t; no-byte-compile: t -*-

;;
;; Bootstrap
;;---------------------------------------------------------------------------
;;

;; hide mode line stuff, enabled earilier to use with use-package
(use-package diminish)

;;
;; Performance
;; ---------------------------------------------------------------------------
;;

;; set up garbage collect
(use-package gcmh
  :diminish 'gcmh-mode
  :load-path "site-lisp/gcmh/"
  :config
  (setq gcmh-high-cons-threshold (* 128 1024 1024))
  (gcmh-mode))

;; read from sub-process in larger chunk
(setq read-process-output-max (* 4 1024 1024))
;; no delay when reading from sub-process
(setq process-adaptive-read-buffering nil)


;; compile lisp libaries to native code asynchronously and load it up when ready
;; (native-compile-async
;;  (list (expand-file-name "site-lisp" user-emacs-directory)
;;        (expand-file-name "lisp" user-emacs-directory)) :recursively :load "\\.el$")


;; save customization to cache directory
(setq custom-file (expand-file-name "custom.el" rangi-emacs-cache-directory))
;; load customization if there is one
(when (file-exists-p custom-file) (load custom-file))





;;
;; Packages
;; ---------------------------------------------------------------------------
;;


;; ;; install into separate directories for each Emacs version, to prevent bytecode incompatibility
;; (let ((versioned-package-dir
;;        (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version) rangi-emacs-cache-directory)))
;;   (setq package-user-dir versioned-package-dir)
;;   (setq package-gnupghome-dir (expand-file-name "gnupg" package-user-dir)))

;;
;; (require 'package)
;;
;; ;; set up archives
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;;
;; ;; compile package during installation
;; (setq package-native-compile t)
;;
;; ;; prefer to load newer version of file if multiple exist
;; (setq load-prefer-newer t)
;;
;; ;; initialize package
;; (package-initialize)
;;
;; ;; set up 'use-package' to initialize packages
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; (eval-when-compile (require 'use-package))
;;
;; ;; always install package if not exist
;; (require 'use-package-ensure)
;; (setq use-package-always-ensure t)
;;
;; ;; update package automatically
;; (use-package auto-package-update
;;   :config
;;   (setq auto-package-update-last-update-day-path
;;         (expand-file-name auto-package-update-last-update-day-filename rangi-emacs-cache-directory))
;;   (setq auto-package-update-delete-old-versions t)
;;   (setq auto-package-update-hide-results t)
;;   (auto-package-update-maybe))
;;



;; ;; add "lisp" directory to load-path
;; (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))



;;
;;
;; ;;
;; ;; Configurations
;; ;; ----------------------------------------------------------------------------
;; ;;
;;
;; ;; some libraries used in the configurations
;; (use-package f)
;; (use-package s)
;; (use-package dash)
;; (use-package async)
;; (use-package hydra)
;;
;; ;; use delight to allow use-package to hide stuff in modeline
;; (use-package delight)
;;
;; ;; load first to set up environment
;; (require 'init-env)
;;
;; ;; rest of the packages and configurations
;; (require 'init-accounting)
;; (require 'init-autosave-and-backup)
;; (require 'init-browse)
;; (require 'init-buffer)
;; (require 'init-copy)
;; (require 'init-devop)
;; (require 'init-dired)
;; (require 'init-docker)
;; (require 'init-editor)
;; (require 'init-emacs-lisp)
;; (require 'init-go)
;; (require 'init-gui)
;; (require 'init-macos)
;; (require 'init-markdown)
;; (require 'init-misc)
;; (require 'init-org)
;; (require 'init-perl)
;; (require 'init-php)
;; (require 'init-prog)
;; (require 'init-project)
;; (require 'init-ruby)
;; (require 'init-rust)
;; (require 'init-shell-and-term)
;; (require 'init-sql)
;; (require 'init-tramp)
;; (require 'init-vc)
;; (require 'init-web)
