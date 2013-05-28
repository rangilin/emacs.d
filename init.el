(add-to-list 'load-path user-emacs-directory)

;; -------------------------------------------------- Initializing
(require 'init-elpa)

;; -------------------------------------------------- modes/features/packages
(require 'init-ido)
(require 'init-misc)
(require 'init-theme)

;; -------------------------------------------------- config keybinding
(require 'init-keybinding)
