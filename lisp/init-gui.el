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

(use-package doric-themes
  :ensure t
  :pin gnu)

(setq rangi-theme-light 'doric-beach)
(setq rangi-theme-dark 'doric-valley)
(setq rangi-theme-current nil)

(defun rangi-load-theme (theme)
  (setq rangi-theme-current theme)
  (load-theme theme t)
  (rangi-theme-set-faces))

(defun rangi-theme-set-faces ()
  ;; header-line
  (set-face-attribute 'header-line nil :box `(:line-width 5 :color ,(face-background 'header-line)) :background (face-background 'header-line))

  ;; mode-line
  (set-face-attribute 'mode-line-active nil :box `(:line-width 5 :color ,(face-background 'mode-line)))
  (set-face-attribute 'mode-line-inactive nil :box `(:line-width 5 :color ,(face-background 'mode-line-inactive))))

(defun rangi-load-theme-according-to-time ()
  (let ((hour (string-to-number (format-time-string "%H"))))
    (if (and (>= hour 8) (<= hour 18))
        (rangi-load-theme rangi-theme-light)
	    (rangi-load-theme rangi-theme-dark))))

(defun rangi-toggle-theme ()
  (interactive)
  (if (or (not rangi-theme-current) (eq rangi-theme-current rangi-theme-dark))
      (rangi-load-theme rangi-theme-light)
    (rangi-load-theme rangi-theme-dark)))

(rangi-load-theme-according-to-time)
(bind-key "C-c e t" 'rangi-toggle-theme)


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

(defvar-keymap rangi-window-prefix-map
  :repeat t
  "<up>" #'windmove-up
  "<down>" #'windmove-down
  "<left>" #'windmove-left
  "<right>" #'windmove-right
  "C-S-<up>" #'windmove-swap-states-up
  "C-S-<down>" #'windmove-swap-states-down
  "C-S-<left>" #'windmove-swap-states-left
  "C-S-<right>" #'windmove-swap-states-right
  "S-<up>" #'windmove-delete-up
  "S-<down>" #'windmove-delete-down
  "S-<right>" #'windmove-delete-right
  "S-<left>" #'windmove-delete-left
  "o" 'other-window
  "O" 'rangi-window-previous)

(bind-key "s-o" 'other-window)
(bind-key "C-x O" 'rangi-window-previous)
(bind-key "s-O" 'rangi-window-previous)
(bind-key "C-c w" rangi-window-prefix-map)
(bind-key "s-w" rangi-window-prefix-map)


;; add built-in window swap in existing window keymap
(keymap-set window-prefix-map "x" 'window-swap-states)
;; auto balance windows
(setq window-combination-resize 1)

;; select the new window after split
(defun rangi-split-window-below ()
  (interactive)
  (select-window (call-interactively 'split-window-below)))
(defun rangi-split-window-right ()
  (interactive)
  (select-window (call-interactively 'split-window-right)))
(defun rangi-split-root-window-below ()
  (interactive)
  (select-window (call-interactively 'split-root-window-below)))
(defun rangi-split-root-window-right ()
  (interactive)
  (select-window (call-interactively 'split-root-window-right)))

(bind-key "C-x 2" 'rangi-split-window-below)
(bind-key "C-x 3" 'rangi-split-window-right)
(bind-key "C-x w 2" 'rangi-split-root-window-below)
(bind-key "C-x w 3" 'rangi-split-root-window-right)



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

  ;; don't show help text
  (setq completion-show-help nil)

  ;; change default completion styles
  (setq completion-styles '(partial-completion substring initials)))

;; only show commands not excluded in current mode
(setq read-extended-command-predicate #'command-completion-default-include-p)

(provide 'init-gui)
