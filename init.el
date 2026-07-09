;;; init.el --- Emacs initialization file -*- lexical-binding: t; no-byte-compile: t -*-


;;;;;;;;;;;;;;;
;; Bootstrap ;;
;;;;;;;;;;;;;;;

;; enable more information when --debug-init
(when init-file-debug
  (setq debug-on-error t
	use-package-verbose t
	use-package-expand-minimally nil))


;; set up path info from shell environment to emacs.
;; this package is installed using local file to avoid having problems install packages
;; when Elpa PGP key expried because it cannot find GnuPG in the PATH
(use-package exec-path-from-shell
  :load-path "site-lisp/exec-path-from-shell"
  :config
  ;; for debugging
  ;; (setq exec-path-from-shell-debug t)
  ;; use non-interactive shell
  (setq exec-path-from-shell-arguments nil)
  ;; load env from shell
  (exec-path-from-shell-initialize))


;; set up package system so we can use it for the rest of the configuration
(use-package package
  :demand t
  :init
  ;; make packages installed in separated directories for each Emacs version so that we can have a clean install
  (setq package-user-dir (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version) rangi-emacs-cache-directory))
  ;; store repository gnupg keys in cache
  (setq package-gnupghome-dir (expand-file-name "gnupg" rangi-emacs-cache-directory))

  :config
  ;; prefer to load newer version of file if multiple exist
  (setq load-prefer-newer t)
  ;; make built-in package upgradeable
  (setq package-install-upgrade-built-in t)

  ;; compile package into native code
  (setq package-native-compile t)
  (setq use-package-compute-statistics t)
  (setq use-package-expand-minimally t)

  (package-initialize))


;; Helper package for keep elpa gnupg pub key up-to-date.
;; When key expired in the future, upgrade this package should automatically fixed it.
;; Signature check is turned off temporarily to avoid chicken and egg problem, where
;; Emacs needs pub key to verify the package but it need the package to get pub key.
(let ((package-check-signature nil))
  (use-package gnu-elpa-keyring-update
    :ensure t
    :config
    ;; fresh install will not have the timestamp file, so a force update is needed
    (unless (file-exists-p (expand-file-name "gnu-elpa.timestamp" package-gnupghome-dir))
      (gnu-elpa-keyring-update))))


;; hide mode line stuff, loaded earilier to use with use-package
(use-package diminish
  :ensure t
  :pin gnu)


;;;;;;;;;;;;;;;;;
;; Performance ;;
;;;;;;;;;;;;;;;;;

;; read from sub-process in larger chunk
(setq read-process-output-max (* 4 1024 1024))

;; no delay when reading from sub-process
(setq process-adaptive-read-buffering nil)

;; defer fontification until input is stopped
(setq jit-lock-defer-time 0)


;;;;;;;;;;;;;;;;;;;;
;; Configurations ;;
;;;;;;;;;;;;;;;;;;;;

;; save customization to cache directory, and load it if exist
(setq custom-file (expand-file-name "custom.el" rangi-emacs-cache-directory))
(when (file-exists-p custom-file) (load custom-file))

;; add configuration files to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; ;; load these first
(require 'init-keybind)
(require 'init-func)

;; load the rest of the configurations
(require 'init-editor)
(require 'init-file)
(require 'init-gui)
(require 'init-misc)
(require 'init-prog)
(require 'init-project)
(require 'init-org)
