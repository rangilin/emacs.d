(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (concat user-emacs-directory (convert-standard-filename "vendor/")))

;; -------------------------------------------------- Initializing
(require 'init-elpa)

;; -------------------------------------------------- modes/features/packages
(require 'init-editing)
(require 'init-gui-frames)
(require 'init-ibuffer)
(require 'init-ido)
(require 'init-ruby-mode)
(require 'init-theme)
(require 'init-webmacro-mode)
(require 'init-window)

(require 'init-misc)
;; -------------------------------------------------- config keybinding
(require 'init-keybinding)

;; -------------------------------------------------- load customization
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
