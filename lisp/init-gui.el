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


;; split window vertically if window width is more than 80, otherwise do it horizontally
(setq split-height-threshold nil)
(setq split-width-threshold 120)



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

;; web mode
(with-eval-after-load 'web-mode
  (set-face-attribute 'web-mode-current-element-highlight-face nil
                      :foreground "orchid1"
                      :background (face-attribute 'default :background)
                      :weight 'bold)
  (set-face-attribute 'web-mode-current-column-highlight-face nil
                      :background "gray30"))




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
