;;; init-gui.el --- GUI configuration file -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous GUI settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; default frame is maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; dynamic frame title
(setq frame-title-format
      '((:eval (let ((project (project-current)))
                 (if project (format "(%s) " (project-name project)) "")))
        "%@ "
        (:eval (if buffer-file-name
                   buffer-file-name
                 "%b"))))

;; turn off these
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)

;; show line/column number on modeline
(column-number-mode 1)
(line-number-mode t)
(size-indication-mode 1)
(setq-default mode-line-position-column-line-format '(" (%l,%C)"))

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
  :demand t
  :bind (("C-c t t" . modus-themes-toggle))
  ;; :init (modus-themes-include-derivatives-mode 1)
  :config
  ;; load derived themes
  ;; (require 'ef-themes (expand-file-name "site-lisp/ef-themes/ef-themes.el" user-emacs-directory))

  ;; assign my light/dark theme for toggling
  (setq rangi-theme-light 'modus-operandi)
  (setq rangi-theme-dark 'modus-vivendi)
  (setq modus-themes-to-toggle `(,rangi-theme-light ,rangi-theme-dark))

  ;; do my own customization after theme is loaded
  (defun rangi-themes-custom-faces (&rest _)
    (modus-themes-with-colors
      (custom-set-faces
       ;; increase header line and mode line padding
       `(header-line ((,c :box (:line-width 5 :color ,(face-background 'header-line)))))
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
	      ;; (after-color (face-attribute 'default :background))
	      (after-color "#db3316"))

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


;; moving point between windows
(defun rangi-window-previous ()
  "Switch to the previous window"
  (interactive)
  (other-window -1))

(defvar-keymap rangi-window-repeat-map
  :repeat t
  "o" #'other-window
  "O" #'rangi-window-previous
  "k" #'windmove-up
  "j" #'windmove-down
  "l" #'windmove-right
  "h" #'windmove-left)

(put 'other-window 'repeat-map 'rangi-window-repeat-map)
(put 'rangi-window-previous 'repeat-map 'rangi-window-repeat-map)

(bind-key "C-x O" 'rangi-window-previous)
(bind-key "C-o" 'other-window)
(bind-key "C-O" 'rangi-window-previous)


;; add built-in window swap in existing window keymap
(keymap-set window-prefix-map "x" 'window-swap-states)
;; auto balance windows
(setq window-combination-resize 1)


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
  ;; completion case-insensitive
  (setq completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  ;; sort completion vertically
  (setq completions-format "vertical")
  ;; control completion list from minibuffer
  (setq minibuffer-visible-completions t)
  ;; change default completeion styles
  (setq completion-styles '(partial-completion substring initials)))

;; only show commands not excluded in current mode
(setq read-extended-command-predicate #'command-completion-default-include-p)

(provide 'init-gui)
