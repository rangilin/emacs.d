;; Use y/n to confirm dialog.
(fset 'yes-or-no-p 'y-or-n-p)

;; always confirm before exit
(setq-default confirm-kill-emacs 'y-or-n-p)

;; no initial message in scratch buffer.
(setq initial-scratch-message "")

;; no startup  message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "")

;; turn off these UIs
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; show column & line number in mode line
(column-number-mode t)
(line-number-mode t)

;; display path of current buffer in the frame title
(setq-default frame-title-format '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b"))))

;; flash mode line when bell ring
(setq visible-bell nil)
(setq ring-bell-function
      (lambda ()
	(invert-face 'mode-line)
	(run-with-timer 0.05 nil 'invert-face 'mode-line)))

;; stop cursor jumping around while scrolling
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)

;; don't blink cursor
(blink-cursor-mode -1)

;; set up fringe
(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines +1)

;; load theme
(require-package 'zenburn-theme)
(load-theme 'zenburn t)

;; use spaceline
(require-package 'spaceline)
(require 'spaceline-config)
(spaceline-emacs-theme)
(require-package 'spaceline-all-the-icons)

;; use icons on spaceline
(require 'spaceline-all-the-icons)
(with-eval-after-load 'spaceline (spaceline-all-the-icons-theme))

(provide 'init-gui)
