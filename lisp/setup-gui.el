(require 'use-package)
(require 'variables)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(column-number-mode t)
(line-number-mode t)

(setq-default inhibit-splash-screen t)
(setq-default inhibit-startup-message t)
(setq-default initial-scratch-message "")

(setq ring-bell-function
      (lambda ()
        (let ((cursor (face-background 'cursor))
              (bg (face-background 'default)))
          (set-face-background 'cursor bg)
          (set-face-background 'cursor cursor))))

(blink-cursor-mode -1)
(setq-default echo-keystrokes 0.1)

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

;; ------------------------------ modeline position
(use-package modeline-posn
  :config
  (progn
    (size-indication-mode 1)
    (setq-default modelinepos-column-limit 80)))

;; ------------------------------ linum
;; http://whattheemacsd.com//key-bindings.el-01.html
(use-package linum
  :init
  (progn
    (defun goto-line-with-feedback ()
      "Show line numbers temporarily, while prompting for the line number input"
      (interactive)
      (let ((line-numbers-off-p (not linum-mode)))
        (unwind-protect
            (progn
              (when line-numbers-off-p
                (linum-mode 1))
              (call-interactively 'goto-line))
          (when line-numbers-off-p
            (linum-mode -1)))))
    (global-set-key [remap goto-line] 'goto-line-with-feedback)))

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

;; ------------------------------ Load theme
(load-theme rangi/theme t)
;; some customization for sanityinc-tomorrow-eighties theme
(set-face-attribute 'mode-line nil
                    :box '(:line-width 1 :color "#EEE")
                    :foreground "white"
                    :background (face-attribute 'default :background))
(set-face-attribute 'mode-line-inactive nil :background (face-attribute 'default :background))
(set-face-background 'fringe (face-attribute 'default :background))


(provide 'setup-gui)
