(customize-set-variable 'column-number-mode t)
(customize-set-variable 'comment-empty-lines t)
(customize-set-variable 'delete-selection-mode t)
(customize-set-variable 'indent-tabs-mode nil)
(customize-set-variable 'inhibit-splash-screen t)
(customize-set-variable 'line-number-mode t)
(customize-set-variable 'menu-bar-mode nil)
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'show-paren-mode t)
(customize-set-variable 'show-trailing-whitespace t)
(customize-set-variable 'tab-width 2)
(customize-set-variable 'tool-bar-mode nil)
(customize-set-variable 'visible-bell t)
(customize-set-variable 'x-select-enable-clipboard t)

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
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      mouse-wheel-progressive-speed nil)

;; -------------------------------------------------- remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; -------------------------------------------------- dired
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-listing-switches "-aBhl  --group-directories-first")

;; -------------------------------------------------- isearch
(setq isearch-allow-scroll 1)

;; -------------------------------------------------- backup
(setq backup-by-copying t)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)
(make-directory "~/.emacs.d/backups/" t)

;; -------------------------------------------------- fringe
(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines +1)

;; -------------------------------------------------- font
; Test whether width of 2 English chars equals to 1 CJK char
; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa (40 chars)
; 測測測測測測測測測測測測測測測測測測測測 (20 chars)
; あいうえおあいうえおあいうえおあいうえお (20 chars)
;
;; (setq face-font-rescale-alist '(("Microsoft JhengHei" . 1.1)))
;; (set-face-attribute 'default nil :font "Consolas 14")
;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font)
;;                     charset (font-spec :family "Microsoft JhengHei")))

(provide 'config)
