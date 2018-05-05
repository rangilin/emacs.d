;;;; parenthese

;; highlight matching parentheses
(show-paren-mode 1)

;; pairing parenthesis automatically
(electric-pair-mode 1)




;;;; Indentations

;; make it easier to insert new line on specific position
(defun rangi-insert-newline-above ()
  "Insert a newline above the current line."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun rangi-insert-newline-below ()
  "Insert a newline below the current line."
  (interactive)
  (end-of-line)
  (newline)
  (indent-according-to-mode))

(global-set-key (kbd "<M-S-return>") 'rangi-insert-newline-above)
(global-set-key (kbd "<S-return>") 'rangi-insert-newline-below)


;; always indent with space
(setq-default indent-tabs-mode nil)
;; indent 2 space by default
(setq-default tab-width 2)
;; try to complete thing-at-point after current line is indented
(setq tab-always-indent 'complete)




;;;; Cursors

(require-package 'multiple-cursors)
(require 'multiple-cursors)

;; save mc file in autogen directory
(setq-default mc/list-file (expand-file-name "mc-lists.el" rangi-generated-files-directory))

;; use this function to active multiple cursor mode
(defun rangi-active-multiple-cursors (arg)
  (interactive "p")
  (message "Multiple cursors is activated...")
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd ">") 'mc/mark-all-like-this)
     (define-key map (kbd "n") 'mc/mark-next-like-this)
     (define-key map (kbd "N") 'mc/skip-to-next-like-this)
     (define-key map (kbd "M-n") 'mc/unmark-next-like-this)
     (define-key map (kbd "p") 'mc/mark-previous-like-this)
     (define-key map (kbd "P") 'mc/skip-to-previous-like-this)
     (define-key map (kbd "M-p") 'mc/unmark-previous-like-this)
     (define-key map (kbd "l") 'mc/edit-lines)
     (define-key map (kbd "'") 'er/expand-region)
     map)
   t))
(global-set-key (kbd "C->") 'rangi-active-multiple-cursors)




;;;; Selection

;; replaces the selection when typed
(delete-selection-mode 1)


;; Use expand-region
(require-package 'expand-region)
(require 'expand-region)
(global-set-key (kbd "C-'") 'er/expand-region)




;;;; Navigation

;; enable subword mode
(global-subword-mode)
(diminish 'subword-mode)


;; move cursor to top or bottom of the buffer when it cannot be scrolled anymore
(setq-default scroll-error-top-bottom t)


;; smarter back-to-beginning behavior
;; http://www.emacswiki.org/emacs/BackToIndentationOrBeginning
(defun rangi-back-to-indentation-or-beginning ()
  "Back to indentation or beginning of current line"
  (interactive "^")
  (if (bound-and-true-p visual-line-mode)
      (beginning-of-visual-line)
    (when (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line))))

(global-set-key (kbd "C-a") 'rangi-back-to-indentation-or-beginning)


;; recenter in the center of a horizontal line
;; http://stackoverflow.com/a/1249665/554279
(defun rangi-horizontal-recenter ()
  "make the point horizontally centered in the window"
  (interactive)
  (let ((mid (/ (window-width) 2))
        (line-len (save-excursion (end-of-line) (current-column)))
        (cur (current-column)))
    (if (< mid cur) (set-window-hscroll (selected-window) (- cur mid)))))

(global-set-key (kbd "C-S-l") 'rangi-horizontal-recenter)


;; jump between characters/words
(require-package 'avy)
(require 'avy)
(setq avy-background t)


;; jump between windows
(require-package 'ace-window)
(require 'ace-window)
(setq-default aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(global-set-key (kbd "C-x o") 'ace-window)


;; use this function to active jump
(defun rangi-active-cursor-jump (arg)
  (interactive "p")
  (message "Jump: (g):word, (G):character, (l):line")
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "g") 'avy-goto-word-1)
     (define-key map (kbd "G") 'avy-goto-char)
     (define-key map (kbd "l") 'avy-goto-line)
   map)
  t))
(global-set-key (kbd "M-g") 'rangi-active-cursor-jump)




;;;; Whitespaces

;; clean trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; show trailing whitespace in prog-mode
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; hide trailing whiespace in minibuffer
(add-hook 'minibuffer-inactive-mode-hook (lambda () (setq show-trailing-whitespace nil)))


;; delete whitespaces more aggresively
(require-package 'hungry-delete)
(require 'hungry-delete)
(global-hungry-delete-mode)
(diminish 'hungry-delete-mode)




;;;; Yank & Kill

(require-package 'browse-kill-ring)
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)




;;;; Undo & Redo
(require-package 'undo-tree)
(require 'undo-tree)
(add-hook 'after-init-hook 'global-undo-tree-mode)
(with-eval-after-load 'undo-tree
  (diminish 'undo-tree-mode))




;;;; Move stuff
(require-package 'move-text)
(require 'move-text)
(global-set-key (kbd "M-P") 'move-text-up)
(global-set-key (kbd "M-N") 'move-text-down)




(provide 'init-editor)
