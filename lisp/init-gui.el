;; smooth scroll
(setq scroll-margin 10)
(setq scroll-step 1)
(setq next-line-add-newlines nil)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position 1)

;; scroll with mouse
(setq mouse-wheel-follow-mouse t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; turn off these UIs
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; show column & line number in mode line
(column-number-mode t)
(line-number-mode t)

;; display path of current buffer in the frame title
(setq-default frame-title-format '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b"))))

;; no alarm bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; don't blink cursor
(blink-cursor-mode -1)


;; set up fringe
(fringe-mode '(nil . 10))
(setq-default indicate-buffer-boundaries 'right)
(setq-default indicate-empty-lines t)


;; show current line
(global-hl-line-mode)

(provide 'init-gui)
