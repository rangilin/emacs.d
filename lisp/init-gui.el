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

  ;; show buffer name and size on frame title
  (setq frame-title-format '("%b (%I)"))

  ;; show keystrokes right away
  (setq echo-keystrokes 0.1)

  ;; no icon on title
  (setq ns-use-proxy-icon nil)

  ;; unbind keys
  (unbind-key "C-z") ; suspend frame
  (unbind-key "s-p") ; print buffer

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
  "a custom visual bell that change cursor color"
  (let ((frame (selected-frame))
	      (before-color (face-attribute 'cursor :background))
	      (after-color "tomato2"))

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
  (which-key-mode))



;;;;;;;;;;;;;;;
;; Highlight ;;
;;;;;;;;;;;;;;;

;; highlight current line, use lin to improve it in different UI
(use-package lin
  :ensure t
  :pin gnu
  :init
  (global-hl-line-mode)
  :config
  (lin-global-mode 1))



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



(provide 'init-gui)




;; ;;
;; ;; Search Completion
;; ;;----------------------------------------------------------------------------
;; ;;

;; ;; use ivy to do do completion everywhere
;; (use-package ivy
;;   :delight
;;   :bind
;;   (("s-b" . ivy-switch-buffer)
;;    ("C-c C-r" . ivy-resume))
;;   :config
;;   (ivy-mode 1)
;;   (setq enable-recursive-minibuffers t)
;;   (setq ivy-use-virtual-buffers t)
;;   (setq ivy-count-format "%d/%d ")
;;   (setq ivy-height-alist
;;       '((t
;;          lambda (_caller)
;;          (max 5 (min 20 (/ (frame-height) 3)))))))

;; ;; install ivy-hydra for additional hydra keybindings
;; (use-package ivy-hydra)

;; ;; use swiper to search local file
;; (use-package swiper)

;; ;; use ivy for emacs commands
;; (use-package counsel
;;   :bind
;;   (("C-s" . counsel-grep-or-swiper)
;;    ("s-f" . counsel-grep-or-swiper)
;;    ("M-x" . counsel-M-x)
;;    ("s-P" . counsel-M-x)
;;    ("C-x C-f" . rangi-counsel-find-file)
;;    ("s-o" . rangi-counsel-find-file)
;;    ("C-x C-r" . counsel-recentf)
;;    ("<f1> f" . counsel-describe-function)
;;    ("<f1> v" . counsel-describe-variable)
;;    ("<f1> l" . counsel-find-library)
;;    ("<f2> i" . counsel-info-lookup-symbol)
;;    ("<f2> u" . counsel-unicode-char))
;;   :config

;;   (setq counsel-grep-swiper-limit 10000000)

;;   (defun rangi-counsel-find-file (arg)
;;     (interactive "P")
;;     (if arg (counsel-file-jump)
;;       (counsel-find-file))))


;; (use-package ivy-rich
;;   :config
;;   (ivy-rich-mode 1)
;;   ;; use abbrevation file path
;;   (setq ivy-rich-path-style 'abbrev)
;;   ;; cache project info
;;   (ivy-rich-project-root-cache-mode 1)
;;   ;; don't parse remote buffer
;;   (setq ivy-rich-parse-remote-buffer nil))
