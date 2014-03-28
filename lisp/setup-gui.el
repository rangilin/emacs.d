(require 'use-package)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(column-number-mode t)
(line-number-mode t)

(setq-default inhibit-splash-screen t)
(setq-default inhibit-startup-message t)
(setq-default initial-scratch-message "")

(setq-default visible-bell t)
(blink-cursor-mode -1)

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
    (setq-default fci-rule-color "#777")
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
  :bind (("C-c f l" . rotate-frame-anticlockwise)
         ("C-c f j" . rotate-frame-clockwise)
         ("C-c f o" . rotate-frame)
         ("C-c f k" . flop-frame)
         ("C-c f i" . flip-frame)
         ("C-c f m" . transpose-frame)))

;; ------------------------------ fringe
(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines +1)

;; ------------------------------ setting for display time
(setq-default display-time-format "%m/%d %T")
(setq-default display-time-interval 1)


(provide 'setup-gui)
