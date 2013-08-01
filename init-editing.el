(setq-default
 show-trailing-whitespace t
 indent-tabs-mode nil
 tab-width 4
 line-number-mode 1
 column-number-mode 1
 x-select-enable-clipboard t
 comint-scroll-to-bottom-on-output 'all)

;; turn off whitespace visualization in some modes
;; --------------------------------------------------
(dolist (hook '(eshell-mode-hook shell-mode-hook))
  (add-hook hook
            (lambda () (setq show-trailing-whitespace nil))))

;; delete/replace active region when typing
;; --------------------------------------------------
(delete-selection-mode 1)

;; auto refresh buffers
;; --------------------------------------------------
(global-auto-revert-mode 1)
;; also refresh non-file buffer like dired
(defvar global-auto-revert-non-file-buffers t)
;; do it sliently
(defvar auto-revert-verbose nil)

;; show matching parenthesis
;; --------------------------------------------------
(show-paren-mode 1)

;; duplicate-thing
;; --------------------------------------------------
(require-package 'duplicate-thing)
(require 'duplicate-thing)

;; scroll behaviors
;; --------------------------------------------------
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

;; undo-tree
;; --------------------------------------------------
(require-package 'undo-tree)
(require 'undo-tree)
(eval-after-load "undo-tree"
  (define-key undo-tree-map (kbd "C-/") nil))
(global-undo-tree-mode 1)

;; move-text
;; --------------------------------------------------
(require-package 'move-text)
(require 'move-text)

;; expand-region
;; --------------------------------------------------
(require-package 'expand-region)
(require 'expand-region)

;; browse-kill-ring
;; --------------------------------------------------
(require-package 'browse-kill-ring)

;; dabbrev-highlight
;; --------------------------------------------------
(add-to-list 'load-path (expand-file-name "vendor/dabbrev-highlight" user-emacs-directory))
(require 'dabbrev-highlight)


(provide 'init-editing)
