(require 'use-package)

(setq-default tab-width 2)
(setq-default show-trailing-whitespace t)
(setq-default show-paren-mode t)
(setq-default comment-empty-lines t)
(setq-default delete-selection-mode t)
(setq-default indent-tabs-mode nil)
(setq-default x-select-enable-clipboard t)

;; ------------------------------ move text
(defun rl/move-text-up (arg)
  "Move text up, but recenter if at upper part of the window"
  (interactive "*p")
  (move-text-up arg)
  (if (rl--point-is-at-upper-window)
      (recenter-top-bottom (truncate (/ (window-text-height) 2)))))

(defun rl/move-text-down (arg)
  "Move text up, but recenter if at lower part of the window"
  (interactive "*p")
  (move-text-down arg)
  (if (region-active-p)
      (exchange-point-and-mark))
  (if (rl--point-is-at-lower-window)
      (recenter-top-bottom (truncate (/ (window-text-height) 2)))))

(defun rl--get-point-y ()
  "Get Y position of the point related to current window"
  (cdr (nth 6 (posn-at-point))))

(defun rl--point-is-at-upper-window ()
  "Check point is at upper part of current window"
  (<= (rl--get-point-y) (truncate (/ (window-text-height) 2))))

(defun rl--point-is-at-lower-window ()
  "Check point is at lower part of current window"
  (> (rl--get-point-y) (truncate (/ (window-text-height) 2))))

(bind-key "M-P" 'rl/move-text-up)
(bind-key "M-N" 'rl/move-text-down)

;; ------------------------------ insert new line
(defun rl/insert-newline-above ()
  "Insert a newline above the current line."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun rl/insert-newline-below ()
  "Insert a newline below the current line."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

(bind-key "<M-S-return>" 'rl/insert-newline-above)
(bind-key "<S-return>" 'rl/insert-newline-below)

;; ------------------------------ back to indentation or beginning
;; http://www.emacswiki.org/emacs/BackToIndentationOrBeginning
(defun rl/back-to-indentation-or-beginning () (interactive)
  "Back to indentation or beginning of current line"
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

(bind-key "C-a" 'rl/back-to-indentation-or-beginning)

;; ------------------------------ horizontal recenter
;; http://stackoverflow.com/a/1249665/554279
(defun rl/horizontal-recenter ()
  "make the point horizontally centered in the window"
  (interactive)
  (let ((mid (/ (window-width) 2))
        (line-len (save-excursion (end-of-line) (current-column)))
        (cur (current-column)))
    (if (< mid cur) (set-window-hscroll (selected-window) (- cur mid)))))

(bind-key "C-S-l" 'rl/horizontal-recenter)

;; ------------------------------ newline & Indent
(bind-key "RET" 'newline-and-indent)
(bind-key "<M-return>" 'newline-and-indent)

;; ------------------------------ delete word
(defun rl/delete-word (arg)
  "Delete word after cursor without add to kill ring"
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun rl/backward-delete-word (arg)
  "Delete word before cursor without add it to kill ring"
  (interactive "p")
  (rl/delete-word (- arg)))

(bind-key "C-M-h" 'rl/backward-delete-word)
(bind-key "M-d" 'rl/delete-word)

;; ------------------------------ delete character backward
(define-key key-translation-map (kbd "C-h") (kbd "DEL"))

;; ------------------------------ kill line
(define-key key-translation-map (kbd "M-H") (kbd "<C-S-backspace>"))

;; ------------------------------ join line
(defun rl/join-below-line ()
  (interactive)
  (join-line 1))

(bind-key "C-j" 'rl/join-below-line)
(bind-key "C-S-j" 'join-line)

;; ------------------------------ buffer auto revert
(global-auto-revert-mode 1)
(setq-default global-auto-revert-non-file-buffers t)
(setq-default auto-revert-verbose nil)

;; ------------------------------ ace jump
(use-package ace-jump-mode
  :bind (("C-;" . ace-jump-mode)))

;; ------------------------------ browse kill ring
(use-package browse-kill-ring
  :bind (("C-S-y" . browse-kill-ring)))

;; ------------------------------ duplicate thing
(use-package duplicate-thing
  :bind ("C-c d" . duplicate-thing))

;; ------------------------------ expand region
(use-package expand-region
  :bind (("C-'" . er/expand-region)
         ("C-\"" . er/contract-region)))

;; ------------------------------ zapzapzapzap
(use-package misc
  :bind (("M-Z" . zap-to-char)
         ("M-z" . zap-up-to-char)))

;; ------------------------------ multiple cursors
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-S-c C->" . mc/mark-more-like-this-extended)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;; ------------------------------ sp
(use-package smartparens
  :diminish ""
  :init
  (progn
    (use-package smartparens-config)
    (use-package smartparens-ruby)
    (use-package smartparens-html)
    (smartparens-global-mode 1)
    (show-smartparens-global-mode 1))
  :config
  (progn
    (setq smartparens-strict-mode t)
    (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))
    (sp-local-pair 'nxml-mode "<" nil :actions :rem)))

;; ------------------------------ undo
(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (progn
    (bind-key "C-/" nil undo-tree-map)
    (bind-key "C-?" nil undo-tree-map)
    (global-undo-tree-mode 1)))

;; ------------------------------ yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (progn
    (let ((snippets-dir (f-expand "snippets" user-emacs-directory)))
      (yas-load-directory snippets-dir)
      (setq-default yas/snippet-dirs snippets-dir))
    (yas-global-mode 1)
    (setq-default yas/prompt-functions '(yas/ido-prompt))))

;; ------------------------------
(bind-key "C-`" 'set-mark-command)

(provide 'setup-editing)
