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

; from https://gist.github.com/coldnew/7398845
(defvar emacs-english-font "Source Code Pro")
(defvar emacs-cjk-font "WenQuanYi Micro Hei")
(defvar emacs-font-size-pair '(17 . 20)
  "Default font size pair for (english . chinese)")
(defvar emacs-font-size-pair-list
  '(( 5 . 6) (10 . 12)
    (13 . 16) (15 . 18) (17 . 20)
    (19 . 22) (20 . 24) (21 . 26)
    (24 . 28) (26 . 32) (28 . 34)
    (30 . 36) (34 . 40) (36 . 44))
  "This list is used to store matching (englis . chinese) font-size.")

(defun font-exist-p (fontname)
  "test if this font is exist or not."
  (if (or (not fontname) (string= fontname ""))
      nil
    (if (not (x-list-fonts fontname))
        nil t)))

(defun set-font (english chinese size-pair)
  "Setup emacs English and Chinese font on x window-system."
  (if (font-exist-p english)
      (set-frame-font (format "%s:pixelsize=%d" english (car size-pair)) t))
  (if (font-exist-p chinese)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font) charset
                          (font-spec :family chinese :size (cdr size-pair))))))

(defun emacs-step-font-size (step)
  "Increase/Decrease emacs's font size."
  (let ((scale-steps emacs-font-size-pair-list))
    (if (< step 0) (setq scale-steps (reverse scale-steps)))
    (setq emacs-font-size-pair
          (or (cadr (member emacs-font-size-pair scale-steps))
              emacs-font-size-pair))
    (when emacs-font-size-pair
      (message "emacs font size set to %.1f" (car emacs-font-size-pair))
      (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair))))

(defun increase-emacs-font-size ()
  "Decrease emacs's font-size acording emacs-font-size-pair-list."
  (interactive) (emacs-step-font-size 1))

(defun decrease-emacs-font-size ()
  "Increase emacs's font-size acording emacs-font-size-pair-list."
  (interactive) (emacs-step-font-size -1))

(set-font emacs-english-font emacs-cjk-font emacs-font-size-pair)

(provide 'config)
