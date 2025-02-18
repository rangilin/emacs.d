;;; init.el --- Emacs initialization file -*- lexical-binding: t; no-byte-compile: t -*-

;;
;; Bootstrap
;;---------------------------------------------------------------------------
;;

;; hide mode line stuff, enabled earilier to use with use-package
(use-package diminish
  :load-path "site-lisp/diminish")



;;
;; Performance
;; ---------------------------------------------------------------------------
;;

;; set up garbage collect
(use-package gcmh
  :diminish 'gcmh-mode
  :load-path "site-lisp/gcmh"
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



;;
;; Environments
;; ---------------------------------------------------------------------------
;;

;; set up path info from shell environment to emacs
(use-package exec-path-from-shell
  :load-path "site-lisp/exec-path-from-shell"
  :config
  ;; use non-interactive shell
  (setq exec-path-from-shell-arguments nil)
  ;; load env from shell
  (exec-path-from-shell-initialize))



;;
;; Configurations
;; ---------------------------------------------------------------------------
;;

;; save customization to cache directory
(setq custom-file (expand-file-name "custom.el" rangi-emacs-cache-directory))
;; load customization if there is one
(when (file-exists-p custom-file) (load custom-file))


;; prepare to load rest of the configurations
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


(require 'init-gui)










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
