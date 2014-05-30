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

(setq-default visible-bell t)
(blink-cursor-mode -1)

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default frame-title-format
              '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name))
                         "%b"))))

;; default frame size to maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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
    (defun rangi--enable-fci ()
      ;; temporary workaround
      ;; https://github.com/alpaker/Fill-Column-Indicator/issues/46
      (when (not (string= major-mode "web-mode"))
        (setq fci-rule-column 80)
        (setq fci-rule-width 2)
        (setq fci-rule-color "#FFF")
        (fci-mode)))
    (add-hook 'prog-mode-hook 'rangi--enable-fci)
    (add-hook 'markdown-mode-hook 'rangi--enable-fci)
    (add-hook 'org-mode-hook 'rangi--enable-fci)))

;; ------------------------------ fullscreen
(use-package fullscreen-mode
  :init (fullscreen-mode 1))

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

;; ------------------------------ workaround emacsclient theme bug
(defun rangi/reload-theme (&rest frame)
  (when window-system
    (let ((theme rangi/theme))
      (message "Reloading theme %s" theme)
      (load-theme theme))))

(defadvice server-create-window-system-frame
  (after reload-theme-on-frame-created ())
  "Reload theme when a frame is created"
  (rangi/reload-theme))

(ad-activate 'server-create-window-system-frame)
(add-hook 'after-make-frame-functions 'rangi/reload-theme t)

(provide 'setup-gui)
