(defun rl-init-module-gui ()
  "Initialize GUI module."
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

  ;; setup locale
  (set-locale-environment "zh_TW.utf-8")

  ;; no splash message
  (setq inhibit-startup-message t)
  (setq inhibit-startup-echo-area-message "")

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
  (use-package spacemacs-theme :ensure t)
  (load-theme 'spacemacs-dark t))


(defun rl--set-up-cursor ()
  (blink-cursor-mode -1))


(defun rl--set-up-mode-line ()
  (use-package spaceline
    :ensure t
    :config
    (require 'spaceline-config)
    (spaceline-spacemacs-theme)))



(provide 'module-gui)
