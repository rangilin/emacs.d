;; show keystroke right away
(setq echo-keystrokes 0.1)

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


;; no alarm bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)


;; stop cursor jumping around while scrolling
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)


;; don't blink cursor
(blink-cursor-mode -1)


;; set up fringe
(fringe-mode '(nil . 12))
(setq-default indicate-buffer-boundaries 'right)
(setq-default indicate-empty-lines t)


;; split window vertically if window width is more than 80, otherwise do it horizontally
(setq split-height-threshold nil)
(setq split-width-threshold 120)


;; show current line
(global-hl-line-mode)



;;; frame/window/buffer arrangement
(require-package 'buffer-move)
(require-package 'transpose-frame)

(defhydra hydra-fwb-arrangement (:color amaranth :hint nil)
  "Arranging window & buffer & frame ..."

  ("_" shrink-window-horizontally)
  ("+" enlarge-window-horizontally)
  ("-" shrink-window)
  ("=" enlarge-window)

  ("J" buf-move-down)
  ("K" buf-move-up)
  ("L" buf-move-right)
  ("H" buf-move-left)

  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("h" windmove-left)

  ("d" rotate-frame-clockwise)
  ("a" rotate-frame-anticlockwise)
  ("w" flip-frame)
  ("s" flop-frame)
  ("e" transpose-frame)

  ("q" nil "Quit"))

(global-set-key (kbd "C-c a") 'hydra-fwb-arrangement/body)





;;;; which-key
(require-package 'which-key)

(which-key-mode)
(which-key-setup-side-window-right-bottom)
(diminish 'which-key-mode)





(provide 'init-gui)
