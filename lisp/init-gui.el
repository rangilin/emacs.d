;;
;; Random stuff
;;----------------------------------------------------------------------------
;;

;; stop cursor jumping around while scrolling
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)

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



;;
;; Themes
;;----------------------------------------------------------------------------
;;

(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-eighties t))

;; theme customizations
(let ((fg (face-attribute 'default :foreground))
      (bg (face-attribute 'default :background)))

  ;; increase mode line height
  (set-face-attribute 'mode-line nil :box `(:line-width 5 :color ,(face-attribute 'mode-line :background)))
  (set-face-attribute 'mode-line-inactive nil :box `(:line-width 5 :color ,(face-attribute 'mode-line-inactive :background)))

  ;; make fringe looks like part of the buffer
  (set-face-background 'fringe bg)

  (with-eval-after-load 'org
    (set-face-attribute 'org-level-1 nil :height 1.0)
    (set-face-attribute 'org-level-2 nil :height 1.0)
    (set-face-attribute 'org-level-3 nil :height 1.0)))


(provide 'init-gui)
