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
(hl-line-mode t)




;;;; golden ratio
(require-package 'golden-ratio)
(require 'golden-ratio)
(golden-ratio-mode 1)

;; https://github.com/roman/golden-ratio.el/issues/68
(defvar golden-ratio-selected-window
  (frame-selected-window)
  "Selected window.")

(defun golden-ratio-set-selected-window
    (&optional window)
  "Set selected window to WINDOW."
  (setq-default
    golden-ratio-selected-window (or window (frame-selected-window))))

(defun golden-ratio-selected-window-p
    (&optional window)
  "Return t if WINDOW is selected window."
  (eq (or window (selected-window))
      (default-value 'golden-ratio-selected-window)))

(defun golden-ratio-maybe
    (&optional arg)
  "Run `golden-ratio' if `golden-ratio-selected-window-p' returns nil."
  (interactive "p")
  (unless (golden-ratio-selected-window-p)
    (golden-ratio-set-selected-window)
    (golden-ratio arg)))

(add-hook 'buffer-list-update-hook #'golden-ratio-maybe)
(add-hook 'focus-in-hook #'golden-ratio)
(add-hook 'focus-out-hook #'golden-ratio)





;;;; which-key
(require-package 'which-key)

(which-key-mode)
(which-key-setup-side-window-right-bottom)
(diminish 'which-key-mode)





;;;; theme
(require-package 'color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-eighties t)

;; avy
(set-face-attribute 'avy-lead-face nil :foreground "red" :background "#2d2d2d" :weight 'bold)
(set-face-attribute 'avy-lead-face-0 nil :foreground "DeepSkyBlue1" :background "#2d2d2d" :weight 'bold)
(set-face-attribute 'avy-lead-face-1 nil :foreground "yellow" :background "#2d2d2d" :weight 'bold)
(set-face-attribute 'avy-lead-face-2 nil :foreground "orange" :background "#2d2d2d" :weight 'bold)

;; ace window faces
(set-face-attribute 'aw-leading-char-face nil :foreground "red" :weight 'extra-bold :height 200)

;; change selection
(set-face-attribute 'region nil :background "#444444")

;; make fringe looks like part of the buffer
(set-face-background 'fringe (face-attribute 'default :background))

;; set web mode highlight
(with-eval-after-load 'web-mode
  (set-face-attribute 'web-mode-current-element-highlight-face nil
                      :foreground "orchid1"
                      :background (face-attribute 'default :background)
                      :weight 'bold)
  (set-face-attribute 'web-mode-current-column-highlight-face nil
                      :background "gray30"))

;; make trailing whitespace more clear
(set-face-attribute 'trailing-whitespace nil :background  "#771313")

;; change current highlight line color
(set-face-attribute 'hl-line nil :background "gray20")

;; change cursor color
(set-face-attribute 'cursor nil :background "gray80")





;;;; mode line
;; don't inherit mode-line face so we can change mode-line easily
(set-face-attribute 'mode-line-inactive nil
                    :inherit nil
                    :box '(:line-width 6 :color "#393939"))


;; change mode-line dynamically
(defun rangi-set-mode-line ()
  (if (file-remote-p default-directory)
      (progn
        (set-face-attribute 'mode-line nil :background "OrangeRed4" :box '(:line-width 6 :color "OrangeRed4")))
    (progn
      (set-face-attribute 'mode-line nil :background "RoyalBlue4" :box '(:line-width 6 :color "RoyalBlue4")))))

(rangi-set-mode-line)
(add-hook 'find-file-hook 'rangi-set-mode-line)
(add-hook 'dired-mode-hook 'rangi-set-mode-line)




(provide 'init-gui)
