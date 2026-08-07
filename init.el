;;; init.el --- Emacs initialization file -*- lexical-binding: t; no-byte-compile: t -*-

;; Tasks:
;; TODO: rethink how gptel access API key without me confirming access from OP every single time
;; TODO: check how to make it easier to save gptel session file, like save will pre-fill default path/file names
;; TODO: check how to make it easier to restore session file (auto insert # -*- eval: (gptel-mode 1) -*- )
;; TODO: use org file for llm session for better formatting ? how to do better formatting ?
;; TODO: can I shorten buffer name in modeline if it's too long (those in llm sessions are long)
;; TODO: better ibuffer group (idea: VC)
;; TODO: how to apply "tabs for indentation, spaces for alignment"
;; TODO: check config https://github.com/zHaOdANiuu/.emacs.d
;; TODO: check config https://github.com/skunkdog/emacs-config/
;; TODO: check config https://codeberg.org/ashton314/emacs-bedrock
;; TODO: check https://github.com/svaante/dape


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


;; Helper package for keep elpa GNUPG pub key up-to-date.
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


;; hide mode line stuff, loaded earlier to use with use-package
(use-package diminish
  :ensure t
  :pin gnu)


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
(require 'init-ai)
(require 'init-editor)
(require 'init-file)
(require 'init-gui)
(require 'init-misc)
(require 'init-prog)
(require 'init-project)
(require 'init-org)
