(add-to-list 'load-path user-emacs-directory)

;; -------------------------------------------------- Initializing
(require 'init-elpa)

;; -------------------------------------------------- modes/features/packages
(require 'init-ido)
(require 'init-misc)
(require 'init-theme)

;; -------------------------------------------------- config keybinding
(require 'init-keybinding)

;; load customize last
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
