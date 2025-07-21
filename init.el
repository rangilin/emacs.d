;;; init.el --- Emacs initialization file -*- lexical-binding: t; no-byte-compile: t -*-


;;;;;;;;;;;;;;;
;; Bootstrap ;;
;;;;;;;;;;;;;;;

;; enable more information when --debug-init
(when init-file-debug
  (setq debug-on-error t
	use-package-verbose t
	use-package-expand-minimally nil
	use-package-compute-statistics t))


;; set up package system so we can use it for the rest of the configuration
(use-package package
  :init
  ;; make packages installed in separated directories for each Emacs version so that we can have a clean install
  (setq package-user-dir (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version) rangi-emacs-cache-directory))
  ;; store repository gnupg keys in cache
  (setq package-gnupghome-dir (expand-file-name "gnupg" package-user-dir))
  :config
  ;; prefer to load newer version of file if multiple exist
  (setq load-prefer-newer t)
  ;; compile package into native code
  (setq package-native-compile t)
  (package-initialize))


;; hide mode line stuff, loaded earilier to use with use-package
(use-package diminish
  :ensure t
  :pin gnu)


;;;;;;;;;;;;;;;;;
;; Performance ;;
;;;;;;;;;;;;;;;;;

;; use garbage collect magic hack
(use-package gcmh
  :diminish 'gcmh-mode
  :load-path "site-lisp/gcmh"
  :config
  (setq gcmh-high-cons-threshold (* 128 1024 1024))
  (gcmh-mode))


;; some miscellaneous stuff
(use-package emacs
  :config
  ;; read from sub-process in larger chunk
  (setq read-process-output-max (* 4 1024 1024))
  ;; no delay when reading from sub-process
  (setq process-adaptive-read-buffering nil)
  ;; defer fontification until input is stopped
  (setq jit-lock-defer-time 0)
  ;; fast scroll
  (setq fast-but-imprecise-scrolling t)
  ;; compile lisp libaries to native code asynchronously and load it up when ready
  (when (native-comp-available-p)
    (native-compile-async
     (list (expand-file-name "site-lisp" user-emacs-directory)
	   (expand-file-name "lisp" user-emacs-directory)) :recursively :load "\\.el$")))



;;;;;;;;;;;;;;;;;;
;; Environments ;;
;;;;;;;;;;;;;;;;;;

;; set up path info from shell environment to emacs
(use-package exec-path-from-shell
  :ensure t
  :pin nongnu
  :config
  ;; use non-interactive shell
  (setq exec-path-from-shell-arguments nil)
  ;; load env from shell
  (exec-path-from-shell-initialize))



;;;;;;;;;;;;;;;;;;;;
;; Configurations ;;
;;;;;;;;;;;;;;;;;;;;

;; save customization to cache directory, and load it if exist
(setq custom-file (expand-file-name "custom.el" rangi-emacs-cache-directory))
(when (file-exists-p custom-file) (load custom-file))

;; load the rest of the configurations
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-editor)
(require 'init-file)
(require 'init-gui)
(require 'init-macos)
(require 'init-misc)
(require 'init-prog)
(require 'init-org)



;; ;; some libraries used in the configurations
;; (use-package f)
;; (use-package s)
;; (use-package dash)
;; (use-package async)
;; (use-package hydra)
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
