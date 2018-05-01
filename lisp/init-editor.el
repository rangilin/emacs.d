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




;;;; Multiple Cursors

(require-package 'multiple-cursors)
(require 'multiple-cursors)

(setq-default mc/list-file (expand-file-name "mc-lists.el" rangi-generated-files-directory))

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
     map)
   t))

(global-set-key (kbd "C->") 'rangi-active-multiple-cursors)

(provide 'init-editor)
