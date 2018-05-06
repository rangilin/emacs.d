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
(set-face-background 'fringe (face-attribute 'default :background)) ; make fringe looks like part of the buffer




;;;; which-key
(require-package 'which-key)

(which-key-mode)
(which-key-setup-side-window-right-bottom)
(diminish 'which-key-mode)



;;;; powerline
(require-package 'powerline)
(require 'powerline)

;; set powerline theme
(powerline-default-theme)

;; incraese modeline height
(setq powerline-height 25)

;; fix separator color not matching issue
;; https://github.com/milkypostman/powerline/issues/54
(setq powerline-image-apple-rgb t)

(defface rangi-modeline-remote-file-font-face
  '((t (:background "OrangeRed4" :inherit mode-line-inactive)))
  "Face used when editing remote file")

(defface rangi-modeline-local-file-font-face
  '((t (:background "RoyalBlue4" :inherit mode-line-inactive)))
  "Face used when editing local file")

(defun rangi-modeline-primary-face ()
  (let ((file (buffer-file-name)))
    (if (and file (file-remote-p file))
        'rangi-modeline-remote-file-font-face
      'rangi-modeline-local-file-font-face)))


;; customized mode line format based on powerline
(setq-default mode-line-format
              '("%e"
                (:eval
                 (let*
                     ((active
                       (powerline-selected-window-active))
                      (mode-line-buffer-id
                       (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                      (mode-line
                       (if active 'mode-line 'mode-line-inactive))
                      (face0
                       (if active 'powerline-active0 'powerline-inactive0))
                      (face1
                       (if active 'powerline-active1 'powerline-inactive1))
                      (face2
                       (if active 'powerline-active2 'powerline-inactive2))
                      (rangi-primary-face
                       (if active (funcall 'rangi-modeline-primary-face)
                         'powerline-inactive0))
                      (separator-left
                       (intern
                        (format "powerline-%s-%s"
                                (powerline-current-separator)
                                (car powerline-default-separator-dir))))
                      (separator-right
                       (intern
                        (format "powerline-%s-%s"
                                (powerline-current-separator)
                                (cdr powerline-default-separator-dir))))
                      (lhs
                       (list
                        (powerline-raw "%*" rangi-primary-face 'l)
                        (when powerline-display-buffer-size
                          (powerline-buffer-size rangi-primary-face 'l))
                        (when powerline-display-mule-info
                          (powerline-raw mode-line-mule-info rangi-primary-face 'l))
                        (powerline-buffer-id
                         `(mode-line-buffer-id ,rangi-primary-face)
                         'l)
                        (when
                            (and
                             (boundp 'which-func-mode)
                             which-func-mode)
                          (powerline-raw which-func-format rangi-primary-face 'l))
                        (powerline-raw " " rangi-primary-face)
                        (funcall separator-left rangi-primary-face face1)
                        (when
                            (and
                             (boundp 'erc-track-minor-mode)
                             erc-track-minor-mode)
                          (powerline-raw erc-modified-channels-object face1 'l))
                        (powerline-major-mode face1 'l)
                        (powerline-process face1)
                        (powerline-minor-modes face1 'l)
                        (powerline-narrow face1 'l)
                        (powerline-raw " " face1)
                        (funcall separator-left face1 face2)
                        (powerline-vc face2 'r)
                        (when
                            (bound-and-true-p nyan-mode)
                          (powerline-raw
                           (list
                            (nyan-create))
                           face2 'l))))
                      (rhs
                       (list
                        (powerline-raw global-mode-string face2 'r)
                        (funcall separator-right face2 face1)
                        (unless window-system
                          (powerline-raw
                           (char-to-string 57505)
                           face1 'l))
                        (powerline-raw "%4l" face1 'l)
                        (powerline-raw ":" face1 'l)
                        (powerline-raw "%3c" face1 'r)
                        (funcall separator-right face1 rangi-primary-face)
                        (powerline-raw " " rangi-primary-face)
                        (powerline-raw "%6p" rangi-primary-face 'r))))
                   (concat
                    (powerline-render lhs)
                    (powerline-fill face2
                                    (powerline-width rhs))
                    (powerline-render rhs))))))




;;;; theme
(require-package 'color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-night t)

;;; set custom faces

;; avy
(set-face-attribute 'avy-lead-face nil :foreground "red" :background "#2d2d2d")
(set-face-attribute 'avy-lead-face-0 nil :foreground "DeepSkyBlue1" :background "#2d2d2d")
(set-face-attribute 'avy-lead-face-1 nil :foreground "yellow" :background "#2d2d2d")
(set-face-attribute 'avy-lead-face-2 nil :foreground "orange" :background "#2d2d2d")

;; ace window faces
(set-face-attribute 'aw-leading-char-face nil :foreground "red" :weight 'extra-bold :height 200)




(provide 'init-gui)
