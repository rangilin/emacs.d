(add-to-list 'load-path user-emacs-directory)
;; -------------------------------------------------- Load functions
;; https://github.com/magnars/.emacs.d/blob/master/init.el
(dolist (file (directory-files
               (expand-file-name "defuns" user-emacs-directory) t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; -------------------------------------------------- Initializing
(require 'init-elpa)

;; -------------------------------------------------- modes/features/packages
(require 'init-editing)
(require 'init-gui-frames)
(require 'init-ibuffer)
(require 'init-ido)
(require 'init-javascript)
(require 'init-linum)
(require 'init-org-mode)
(require 'init-ruby-mode)
(require 'init-shell)
(require 'init-theme)
(require 'init-vc)
(require 'init-web)
(require 'init-webmacro-mode)
(require 'init-window)

(require 'init-misc)

(require 'init-local nil t) ; local settings goes here
;; -------------------------------------------------- config keybinding
(require 'init-keybinding)

;; -------------------------------------------------- load customization
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
