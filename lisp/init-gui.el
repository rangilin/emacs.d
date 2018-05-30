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





;;;; theme

(require-package 'color-theme-sanityinc-tomorrow)

(defun rangi-set-common-face-attribute ()
  (let ((fg (face-attribute 'default :foreground))
        (bg (face-attribute 'default :background)))

    ;; make ace window faces clear
    (set-face-attribute 'aw-leading-char-face nil :foreground "red" :weight 'extra-bold :height 300)

    ;; make fringe looks like part of the buffer
    (set-face-background 'fringe bg)

    ;; set web mode highlight
    (with-eval-after-load 'web-mode
      (set-face-attribute 'web-mode-current-element-highlight-face nil
                          :foreground "MediumOrchid1"
                          :background (face-attribute 'default :background)
                          :weight 'bold))

    (set-face-attribute 'avy-lead-face nil :foreground "dark orange" :background bg :weight 'bold)
    (set-face-attribute 'avy-lead-face-0 nil :foreground "DarkOrchid1" :background bg :weight 'bold)
    (set-face-attribute 'avy-lead-face-1 nil :foreground "red" :background bg :weight 'bold)
    (set-face-attribute 'avy-lead-face-2 nil :foreground "deep pink" :background bg :weight 'bold)

    ;; increase mode line height
    (set-face-attribute 'mode-line nil :box `(:line-width 5 :color ,(face-attribute 'mode-line :background)))
    (set-face-attribute 'mode-line-inactive nil :box `(:line-width 5 :color ,(face-attribute 'mode-line-inactive :background)))))


(defun rangi-set-theme-sanityinc-tomorrow-eighties ()
  (load-theme 'sanityinc-tomorrow-eighties t)
  (rangi-set-common-face-attribute)


  ;; make trailing whitespace more clear
  (set-face-attribute 'trailing-whitespace nil :background "#771313")
  ;; change selection
  (set-face-attribute 'region nil :background "#444444")
  ;; change current highlight line color
  (set-face-attribute 'hl-line nil :background "gray20")
  ;; change cursor color
  (set-face-attribute 'cursor nil :background "gray80"))


(defun rangi-set-theme-sanityinc-tomorrow-day ()
  (load-theme 'sanityinc-tomorrow-day t)
  (set-face-attribute 'default nil :background "gray97")
  (rangi-set-common-face-attribute)

  ;; set web mode highlight
  (with-eval-after-load 'web-mode
    (set-face-attribute 'web-mode-current-element-highlight-face nil
                        :foreground "MediumOrchid1"
                        :background (face-attribute 'default :background)
                        :weight 'bold))

  ;; make trailing whitespace more clear
  (set-face-attribute 'trailing-whitespace nil :background "IndianRed1")
  ;; change selection
  (set-face-attribute 'region nil :background "#444444")
  ;; change current highlight line color
  (set-face-attribute 'hl-line nil :background "gray92")
  ;; change cursor color
  (set-face-attribute 'cursor nil :background "gray20"))



(rangi-set-theme-sanityinc-tomorrow-eighties)






(provide 'init-gui)
