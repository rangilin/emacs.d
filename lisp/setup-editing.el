(require 'use-package)

(setq-default tab-width 2)
(setq-default comment-empty-lines t)
(setq-default indent-tabs-mode nil)
(setq-default x-select-enable-clipboard t)
(setq-default require-final-newline t)

(delete-selection-mode 1)
(show-paren-mode 1)
(global-subword-mode)

;; ------------------------------ horizontal recenter
;; http://stackoverflow.com/a/1249665/554279
(defun rangi/horizontal-recenter ()
  "make the point horizontally centered in the window"
  (interactive)
  (let ((mid (/ (window-width) 2))
        (line-len (save-excursion (end-of-line) (current-column)))
        (cur (current-column)))
    (if (< mid cur) (set-window-hscroll (selected-window) (- cur mid)))))

(bind-key "C-S-l" 'rangi/horizontal-recenter)

;; ------------------------------ newline & Indent
(bind-key "RET" 'newline-and-indent)
(bind-key "<M-return>" 'newline-and-indent)

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

;; ------------------------------ multiple cursors
(use-package multiple-cursors
  :init
  (progn
    (setq-default mc/list-file (expand-file-name ".mc-lists.el" rangi/gen-dir)))
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-S-c C->" . mc/mark-more-like-this-extended)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

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
;; Cursor Movement
;; ============================================================

(defun rangi/backward-whitespace ()
  (interactive)
  (forward-whitespace -1))

(bind-key "M-F" `forward-whitespace)
(bind-key "M-B" `rangi/backward-whitespace)

;; ------------------------------ back to indentation or beginning
;; http://www.emacswiki.org/emacs/BackToIndentationOrBeginning
(defun rangi/back-to-indentation-or-beginning () (interactive)
  "Back to indentation or beginning of current line"
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

(bind-key "C-a" 'rangi/back-to-indentation-or-beginning)

;; ------------------------------ ace jump
(use-package ace-jump-mode
  :bind (("C-;" . ace-jump-mode)))

;; ------------------------------ forward/backward paragraph
;; http://whattheemacsd.com/setup-html-mode.el-01.html
(defun rangi/forward-paragraph ()
  (interactive)
  (let ((inhibit-changing-match-data t))
    (skip-syntax-forward " >")
    (unless (search-forward-regexp "^\\s *$" nil t)
      (goto-char (point-max)))))

(defun rangi/backward-paragraph ()
  (interactive)
  (let ((inhibit-changing-match-data t))
    (skip-syntax-backward " >")
    (unless (search-backward-regexp "^\\s *$" nil t)
      (goto-char (point-min)))))

(bind-key "M-a" 'rangi/backward-paragraph)
(bind-key "M-e" 'rangi/forward-paragraph)

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
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun rangi/insert-newline-below ()
  "Insert a newline below the current line."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

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

(provide 'setup-editing)
