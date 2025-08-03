;;; init-gui.el --- GUI configuration file -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous GUI settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :bind (("s-=" . rangi-what-line-column))
  :config
  ;; show lines and columns on demand
  (defun rangi-what-line-column ()
    (interactive)
    (let ((l (line-number-at-pos))
	        (c (current-column)))
      (message "Line: %d, Column: %d" l c)))

  ;; turn off these
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode -1)
  (column-number-mode -1)
  (line-number-mode -1)

  ;; default frame is maximized
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; show buffer name and size on frame title
  (setq frame-title-format '("%b (%I)"))

  ;; show keystrokes right away
  (setq echo-keystrokes 0.1)

  ;; no icon on title
  (setq ns-use-proxy-icon nil)

  ;; confirm before quit
  (setq confirm-kill-emacs 'y-or-n-p)
  ;; force ask y/n instead of yes/no
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; select help window automatically, so it is easier to close it with `q`
  (setq help-window-select t)

  ;; make initial scratch buffer empty
  (setq initial-scratch-message nil)

  ;; make emacs resize UI by pixel
  (setq window-resize-pixelwise t)
  (setq frame-resize-pixelwise t)

  ;; show no stuff on startup
  (setq inhibit-startup-message t)
  (setq inhibit-startup-echo-area-message nil)
  (setq inhibit-startup-screen t))


;;;;;;;;;;;
;; MacOS ;;
;;;;;;;;;;;

;; set command keys as `super'
(setq mac-command-modifier 'super)
(setq mac-right-command-modifier 'super)

;; set option keys as `meta'
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'meta)

;; use `mdfind' on MacOS instead of `locate'
(setq-default locate-command "mdfind")



;;;;;;;;;;;
;; Theme ;;
;;;;;;;;;;;

(use-package emacs
  :bind (("C-c t t" . modus-themes-toggle))
  :config
  (require-theme 'modus-themes)

  ;; toggle between tinted version of theme
  (setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))

  ;; my customizations
  (defun rangi-modus-themes-custom-faces (&rest _)
    (modus-themes-with-colors
      (custom-set-faces
       `(mode-line ((,c :box (:line-width 5 :color ,bg-mode-line-active))))
       `(mode-line-inactive ((,c :box (:line-width 5 :color ,bg-mode-line-inactive)))))))

  ;; update theme faces after theme loaded
  (add-hook 'modus-themes-after-load-theme-hook #'rangi-modus-themes-custom-faces)


  ;; load theme according to certain criteria
  (defun rangi-load-theme-accordingly ()
    (if (fboundp 'mac-application-state)
	      (rangi-load-theme-according-to-macos)
      (rangi-load-theme-according-to-time)))

  (defun rangi-load-theme-according-to-macos ()
    (let ((appearance (plist-get (mac-application-state) :appearance)))
      (if (string-equal appearance "NSAppearanceNameDarkAqua")
          (modus-themes-load-theme 'modus-vivendi-tinted)
	      (modus-themes-load-theme 'modus-operandi-tinted))))

  (defun rangi-load-theme-according-to-time ()
    (let ((hour (string-to-number (format-time-string "%H"))))
      (if (and (>= hour 8) (<= hour 18))
	        (modus-themes-load-theme 'modus-operandi-tinted)
	      (modus-themes-load-theme 'modus-vivendi-tinted))))

  ;; load theme when macos change appearance, if available
  (when (fboundp 'mac-effective-appearance-change-hook)
    (add-hook 'mac-effective-appearance-change-hook #'rangi-load-theme-accordingly))

  (rangi-load-theme-accordingly))



;;;;;;;;;;
;; Bell ;;
;;;;;;;;;;

(defun rangi-cursor-visual-bell ()
  "a custom visual bell that make cursor blink"
  (let ((frame (selected-frame))
	      (before-color (face-attribute 'cursor :background))
	      (after-color (face-attribute 'default :background)))

    ;; if bell function is called when color is still waiting to be change
    ;; it will cause cursor color remain `after-color'
    ;; so we only switch color when cursor color is not the color we use
    (unless (string-equal before-color after-color)
      (run-with-timer 0.1 nil
		                  #'(lambda (frame)
			                    (let ((inhibit-quit)
				                        (inhibit-redisplay t))
			                      (set-cursor-color before-color))) frame)
      (let ((inhibit-quit)
            (inhibit-redisplay t))
	      (set-cursor-color after-color)))))

(setq visible-bell nil)
(setq ring-bell-function 'rangi-cursor-visual-bell)



;;;;;;;;;;
;; Help ;;
;;;;;;;;;;

(use-package which-key
  :diminish
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))



;;;;;;;;;;;;
;; Window ;;
;;;;;;;;;;;;

;; winner for saving windows configurations
(use-package winner
  :config
  (winner-mode 1))

(use-package windmove
  :bind-keymap ("C-c w" . rangi-windmove-repeat-map)
  :bind
  (:repeat-map rangi-windmove-repeat-map
               ("k" . windmove-up)
               ("j" . windmove-down)
               ("l" . windmove-right)
               ("h" . windmove-left)))


;;;;;;;;;;;;
;; Fringe ;;
;;;;;;;;;;;;

(fringe-mode 9)
(setq-default indicate-buffer-boundaries 'right)
(setq-default indicate-empty-lines t)



;;;;;;;;;;;;;;;;
;; Breadcrumb ;;
;;;;;;;;;;;;;;;;

(use-package breadcrumb
  :diminish
  :ensure t
  :pin gnu
  :config
  (breadcrumb-mode))



;;;;;;;;;;;;;;;;
;; Completion ;;
;;;;;;;;;;;;;;;;

(use-package minibuffer
  :config
  ;; sort completion vertically
  (setq completions-format "vertical")
  ;; control completion list from minibuffer
  (setq minibuffer-visible-completions t)
  ;; change default completeion styles
  (setq completion-styles '(initials partial-completion)))



(provide 'init-gui)
