(defun rl-init-module-gui ()
  "Initialize GUI module."
  (rl--set-up-fringe)
  (rl--set-up-bell)
  (rl--set-up-scroll)
  (rl--set-up-cursor)
  (rl--set-up-theme)
  (rl--set-up-mode-line)
  (rl--set-up-interface))


(defun rl--set-up-interface ()
  "Set up GUI."

  ;; turn off these interface
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)

  ;; Don't print initial message in scratch buffer.
  (setq initial-scratch-message "")

  ;; highlight current line
  (global-hl-line-mode 1)

  ;; Use y/n to confirm dialog.
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; always confirm before exit
  (setq-default confirm-kill-emacs 'y-or-n-p)

  ;; setup locale
  (set-locale-environment "zh_TW.utf-8")

  ;; no splash message
  (setq inhibit-startup-message t)
  (setq inhibit-startup-echo-area-message "")

  ;; display path of current buffer in the frame title
  (setq-default frame-title-format
              '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name))
                         "%b"))))

  ;; show column & line number in mode line
  (column-number-mode t)
  (line-number-mode t))


(defun rl--set-up-bell ()
  "Set up bell."
  (setq visible-bell nil)
  (setq ring-bell-function
        (lambda ()
          (invert-face 'mode-line)
          (run-with-timer 0.05 nil 'invert-face 'mode-line))))


(defun rl--set-up-scroll ()
  "Set up scroll."
  (setq scroll-conservatively 10000)
  (setq scroll-preserve-screen-position t))


(defun rl--set-up-theme ()
  (use-package color-theme-sanityinc-tomorrow :ensure t)
  (load-theme 'sanityinc-tomorrow-eighties t)

  (set-face-background 'fringe (face-attribute 'default :background))
  (set-face-attribute 'trailing-whitespace nil :background "gray35")
  (set-face-attribute 'avy-lead-face nil :foreground "red" :background "#2d2d2d")
  (set-face-attribute 'avy-lead-face-0 nil :foreground "DeepSkyBlue1" :background "#2d2d2d")
  (set-face-attribute 'avy-lead-face-1 nil :foreground "yellow" :background "#2d2d2d")
  (set-face-attribute 'avy-lead-face-2 nil :foreground "orange" :background "#2d2d2d"))


(defun rl--set-up-cursor ()
  (blink-cursor-mode -1))


(defun rl--set-up-mode-line ()
  (use-package spaceline
    :ensure t
    :config
    (require 'spaceline-config)
    (when (fboundp 'spaceline-emacs-theme)
      (spaceline-emacs-theme))))


(defun rl--set-up-fringe ()
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default indicate-empty-lines +1))


(provide 'module-gui)
