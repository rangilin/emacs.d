(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (concat user-emacs-directory (convert-standard-filename "vendor/")))

;; -------------------------------------------------- Initializing
(require 'init-elpa)

;; -------------------------------------------------- modes/features/packages
(require 'init-ace-jump-mode)
(require 'init-editing)
(require 'init-gui-frames)
(require 'init-ibuffer)
(require 'init-ido)
(require 'init-linum)
(require 'init-ruby-mode)
(require 'init-theme)
(require 'init-webmacro-mode)
(require 'init-window)

(require 'init-misc)
;; -------------------------------------------------- config keybinding
(require 'init-keybinding)

;;--------------------------------------------------- Start server
(require 'server)
(unless (server-running-p)
  (server-start))

;; -------------------------------------------------- load customization
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
