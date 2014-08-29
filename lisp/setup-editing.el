(require 'use-package)

(setq-default tab-width 2)
(setq-default comment-empty-lines t)
(setq-default indent-tabs-mode nil)
(setq-default x-select-enable-clipboard t)
(setq-default require-final-newline t)

(delete-selection-mode 1)
(show-paren-mode 1)
(global-subword-mode)

;; ------------------------------ newline & Indent
(bind-key "RET" 'newline-and-indent)

;; ------------------------------ kill line
(define-key key-translation-map (kbd "M-H") (kbd "<C-S-backspace>"))

;; ------------------------------ join line
(defun rangi/join-below-line ()
  (interactive)
  (join-line 1))

(bind-key "C-j" 'rangi/join-below-line)
(bind-key "C-S-j" 'join-line)

;; ------------------------------ browse kill ring
(use-package browse-kill-ring
  :bind (("C-S-y" . browse-kill-ring)))

;; ------------------------------ expand region
(use-package expand-region
  :bind (("C-'" . er/expand-region)
         ("C-\"" . er/contract-region))
  :init
  (progn
    (defun rangi/er-org-mode-hook ()
      (bind-key "C-'" 'er/expand-region org-mode-map))
    (add-hook 'org-mode-hook 'rangi/er-org-mode-hook)))

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

;; ============================================================
;; Text Manipulation
;; ============================================================

(bind-key "C-d" 'delete-forward-char)

;; ------------------------------ delete word
(defun rangi/delete-word (arg)
  "Delete word after cursor without add to kill ring"
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun rangi/backward-delete-word (arg)
  "Delete word before cursor without add it to kill ring"
  (interactive "p")
  (rangi/delete-word (- arg)))

(bind-key "C-M-h" 'rangi/backward-delete-word)
(bind-key "M-d" 'rangi/delete-word)

;; ------------------------------ delete character backward
(define-key key-translation-map (kbd "C-h") (kbd "DEL"))

;; ------------------------------ move text
(defun rangi/move-text-up (arg)
  "Move text up, but recenter if at upper part of the window"
  (interactive "*p")
  (move-text-up arg)
  (if (rangi--point-is-at-upper-window)
      (recenter-top-bottom (truncate (/ (window-text-height) 2)))))

(defun rangi/move-text-down (arg)
  "Move text up, but recenter if at lower part of the window"
  (interactive "*p")
  (move-text-down arg)
  (if (region-active-p)
      (exchange-point-and-mark))
  (if (rangi--point-is-at-lower-window)
      (recenter-top-bottom (truncate (/ (window-text-height) 2)))))

(defun rangi--get-point-y ()
  "Get Y position of the point related to current window"
  (cdr (nth 6 (posn-at-point))))

(defun rangi--point-is-at-upper-window ()
  "Check point is at upper part of current window"
  (<= (rangi--get-point-y) (truncate (/ (window-text-height) 2))))

(defun rangi--point-is-at-lower-window ()
  "Check point is at lower part of current window"
  (> (rangi--get-point-y) (truncate (/ (window-text-height) 2))))

(bind-key "M-P" 'rangi/move-text-up)
(bind-key "M-N" 'rangi/move-text-down)

;; ------------------------------ zapzapzapzap
(use-package misc
  :bind (("M-Z" . zap-to-char)
         ("M-z" . zap-up-to-char)))

;; ------------------------------ duplicator
(use-package duplicator
  :load-path "site-lisp/duplicator"
  :bind ("C-c d" . duplicator/duplicate-lines))

;; ------------------------------ insert new line
(defun rangi/insert-newline-above ()
  "Insert a newline above the current line."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun rangi/insert-newline-below ()
  "Insert a newline below the current line."
  (interactive)
  (end-of-line)
  (newline)
  (indent-according-to-mode))

(bind-key "<M-S-return>" 'rangi/insert-newline-above)
(bind-key "<S-return>" 'rangi/insert-newline-below)

;; ------------------------------ comment
(defun rangi/comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position))
      (deactivate-mark))
    (comment-or-uncomment-region beg end)
    (next-logical-line)))

(bind-key "C-/" 'rangi/comment-or-uncomment-region-or-line)

;; ------------------------------ shuffle lines
(use-package randomize-region
  :load-path "site-lisp/randomize-region")

;; ------------------------------ yank & indent
(defun rangi/yank-and-indent ()
  "Yank and then indent according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))

(bind-key "C-y" 'rangi/yank-and-indent)

(provide 'setup-editing)
