(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)

(line-number-mode 1)
(column-number-mode 1)
(delete-selection-mode 1)
(show-paren-mode 1)

(setq-default x-select-enable-clipboard t)
(setq-default visible-bell 1)
(setq-default show-trailing-whitespace t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq backup-by-copying t)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(fset 'yes-or-no-p 'y-or-n-p)

(require 'comint)
(setq-default comint-scroll-to-bottom-on-output 'all)

;; -------------------------------------------------- trailing whitespace
(dolist (hook '(eshell-mode-hook
		shell-mode-hook
		diff-mode-hook
		comint-mode-hook
		term-mode-hook))
  (add-hook hook (lambda () (setq show-trailing-whitespace nil))))

(defun turn-off-whitespace-mode-by-file-extension ()
  (when (and (stringp buffer-file-name)
             (string-match "\\.log" buffer-file-name))
    (setq show-trailing-whitespace nil)))

(add-hook 'find-file-hook 'turn-off-whitespace-mode-by-file-extension)

;; -------------------------------------------------- buffer auto revert
(global-auto-revert-mode 1)
;; also refresh non-file buffer like dired
(setq-default global-auto-revert-non-file-buffers t)
;; do it sliently
(setq-default auto-revert-verbose nil)

;; -------------------------------------------------- Scrolling
;; mouse scrolling three line at a time
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1)))

;; don't accelerate mouse scrolling
(setq mouse-wheel-progressive-speed nil)

;; scroll window under mouse
(setq mouse-wheel-follow-mouse 't)

;; make emacs scroll one line instead of half screen
;; when cursor meet top/bottom of the screen
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; -------------------------------------------------- remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; -------------------------------------------------- dired
(put 'dired-find-alternate-file 'disabled nil)

;; -------------------------------------------------- isearch
(setq isearch-allow-scroll 1)

(provide 'config)
