;; install themes
(require-package 'color-theme-sanityinc-tomorrow)
(require-package 'solarized-theme)
(require-package 'zenburn-theme)


(defun rangi-load-solarized-dark-theme ()
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil)
  (setq x-underline-at-descent-line t)
  (load-theme 'solarized-dark t))



;; load theme
;; (rangi-load-solarized-dark-theme)

(load-theme 'zenburn t)


;; some customization
(let ((fg (face-attribute 'default :foreground))
      (bg (face-attribute 'default :background)))

  ;; ;; increase mode line height
  (set-face-attribute 'mode-line nil :box `(:line-width 5 :color ,(face-attribute 'mode-line :background)))
  (set-face-attribute 'mode-line-inactive nil :box `(:line-width 5 :color ,(face-attribute 'mode-line-inactive :background)))

  ;; make ace window faces clear
  (set-face-attribute 'aw-leading-char-face nil :foreground "red" :weight 'extra-bold :height 300)

  ;; ;; make fringe looks like part of the buffer
  (set-face-background 'fringe bg)

  (with-eval-after-load 'org
    (set-face-attribute 'org-level-1 nil :height 1.0)
    (set-face-attribute 'org-level-2 nil :height 1.0)
    (set-face-attribute 'org-level-3 nil :height 1.0)))



(provide 'init-theme)
