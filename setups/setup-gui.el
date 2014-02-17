(require 'use-package)


(setq-default menu-bar-mode nil)
(setq-default scroll-bar-mode nil)
(setq-default tool-bar-mode nil)

(setq-default column-number-mode t)
(setq-default line-number-mode t)

(setq-default inhibit-splash-screen t)
(setq-default inhibit-startup-message t)
(setq-default initial-scratch-message "")

(setq-default visible-bell t)
(setq-default blink-cursor-mode nil)

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default frame-title-format
              '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name))
                         "%b"))))

;; ------------------------------ scrolling behavior
(setq-default redisplay-dont-pause t)
(setq-default scroll-margin 1)
(setq-default scroll-step 1)
(setq-default scroll-conservatively 10000)
(setq-default scroll-preserve-screen-position 1)
(setq-default mouse-wheel-progressive-speed nil)

;; ------------------------------ column indicator
(use-package fill-column-indicator
  :init
  (progn
    (setq-default fci-rule-column 80)
    (setq-default fci-rule-width 2)
    (add-hook 'prog-mode-hook 'fci-mode)))

;; ------------------------------ fullscreen
(use-package fullscreen-mode
  :init (fullscreen-mode 1))

;; ------------------------------ linum
(use-package linum
  :init
  (progn
    (defun linum-on ()
      (unless (or (minibufferp)
                  (member major-mode '(shell-mode eshell-mode text-mode dired-mode))
                  (string-match "*" (buffer-name)))
        (linum-mode 1)))
    (global-linum-mode 1)))

;; ------------------------------ transpose frame
(use-package transpose-frame
  :load-path "site-lisp/transpose-frame"
  :init
  (progn
    (define-prefix-command 'transpose-frame-map)
    (bind-key "C-c f" transpose-frame-map)
    (bind-key "l" 'rotate-frame-anticlockwise transpose-frame-map)
    (bind-key "j" 'rotate-frame-clockwise transpose-frame-map)
    (bind-key "o" 'rotate-frame transpose-frame-map)
    (bind-key "k" 'flop-frame transpose-frame-map)
    (bind-key "i" 'flip-frame transpose-frame-map)
    (bind-key "m" 'transpose-frame transpose-frame-map)))

;; ------------------------------ fringe
(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines +1)

(provide 'setup-gui)
