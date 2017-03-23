(defun rl/initialize-module-gui ()
  "Initialize GUI module."
  (rl--set-up-bell)
  (rl--set-up-scroll)
  (rl--set-up-cursor)
  (rl--set-up-theme)
  (rl--set-up-ido)
  (rl--set-up-window)
  (rl--set-up-interface))

(defun rl/split-window-vertically ()
  "Split window vertically. Focus to the new window and display a recent used
buffer in it"
  (interactive)
  (call-interactively 'split-window-vertically)
  (set-window-buffer (next-window) (other-buffer))
  (other-window 1))

(defun rl/split-window-horizontally ()
  "Split window horizontally. Focus to the new window and display a recent used
buffer in it"
  (interactive)
  (call-interactively 'split-window-horizontally)
  (set-window-buffer (next-window) (other-buffer))
  (other-window 1))

(defun rl--set-up-interface ()
  "Set up GUI."

  ;; turn off these interface
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (unless (string= system-type "darwin")
    (menu-bar-mode -1))

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


(defun rl--set-up-ido ()
  (use-package ido
    :config

    (setq ido-enable-flex-matching t)
    (setq ido-use-virtual-buffers t)
    (setq ido-save-directory-list-file (expand-file-name "ido.last" rl/dir-autogen))

    (ido-everywhere 1)
    (ido-mode 1)

    (use-package ido-ubiquitous
      :ensure t
      :config
      (ido-ubiquitous-mode 1))

    (use-package ido-vertical-mode
      :ensure t
      :config
      (setq-default ido-vertical-define-keys 'C-n-and-C-p-only)
      (setq-default ido-vertical-show-count t)
      (ido-vertical-mode 1))

    ;; ido M-x
    (use-package smex
      :ensure t
      :bind
      ("M-x" . smex)
      :config
      (setq smex-save-file (expand-file-name "smex_items" rl/dir-autogen))
      (smex-initialize))))


(defun rl--set-up-window ()
  (bind-key "C-x 2" 'rl/split-window-vertically)
  (bind-key "C-x 3" 'rl/split-window-horizontally))

(provide 'module-gui)
