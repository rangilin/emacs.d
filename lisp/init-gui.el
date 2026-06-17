;;; init-gui.el --- GUI configuration file -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous GUI settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; show lines and columns on demand
(defun rangi-what-line-column ()
  (interactive)
  (let ((l (line-number-at-pos))
	      (c (current-column)))
    (message "Line: %d, Column: %d" l c)))

(bind-key "s-=" 'rangi-what-line-column)


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
(setq inhibit-startup-screen t)


;; better (keyboard-quit)
;; https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/#h:1e468b2a-9bee-4571-8454-e3f5462d9321
(defun rangi-keyboard-quit-dwim ()
  (interactive)
  (cond ((region-active-p)
         (keyboard-quit))
        ((derived-mode-p 'completion-list-mode)
         (delete-completion-window))
        ((> (minibuffer-depth) 0)
         (abort-recursive-edit))
        (t
         (keyboard-quit))))

(bind-key "C-g" 'rangi-keyboard-quit-dwim)



;;;;;;;;;;;
;; Theme ;;
;;;;;;;;;;;

(use-package modus-themes
  :pin gnu
  :ensure t
  :init (modus-themes-include-derivatives-mode 1)
  :bind (("C-c t t" . modus-themes-toggle))
  :config

  ;; load derived themes
  (require 'ef-themes (expand-file-name "site-lisp/ef-themes/ef-themes.el" user-emacs-directory))

  ;; assign my light/dark theme for toggling
  ;; other ef theme pair I use:
  ;; ef-reverie/ef-dream
  (setq rangi-theme-light 'ef-maris-light)
  (setq rangi-theme-dark 'ef-maris-dark)
  (setq modus-themes-to-toggle `(,rangi-theme-light ,rangi-theme-dark))

  ;; do my own customization after theme is loaded
  (defun rangi-themes-custom-faces (&rest _)
    (modus-themes-with-colors
      (custom-set-faces
       `(mode-line-active ((,c :box (:line-width 5 :color ,(face-background 'mode-line-active)))))
       `(mode-line-inactive ((,c :box (:line-width 5 :color ,(face-background 'mode-line-inactive))))))))
  (add-hook 'modus-themes-after-load-theme-hook #'rangi-themes-custom-faces)

  ;; load initial theme based on time of day
  (defun rangi-load-theme-according-to-time ()
    (let ((hour (string-to-number (format-time-string "%H"))))
      (if (and (>= hour 8) (<= hour 18))
          (modus-themes-load-theme rangi-theme-light)
	      (modus-themes-load-theme rangi-theme-dark))))
  (rangi-load-theme-according-to-time))



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

;; backward other-window with repeat enabled
(bind-key "C-x O" (lambda ()
                    (interactive)
                    (setq repeat-map 'other-window-repeat-map)
                    (other-window -1)))

;;;;;;;;;;;;
;; Fringe ;;
;;;;;;;;;;;;

(fringe-mode 9)
(setq-default indicate-buffer-boundaries 'right)
(setq-default indicate-empty-lines t)



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
  (setq completion-styles '(initials partial-completion substring)))



(provide 'init-gui)
