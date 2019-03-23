;; warn when open file larger than 100MB
(setq large-file-warning-threshold 100000000)

;; highlight matching parentheses
(show-paren-mode 1)

;; pairing parenthesis automatically
(electric-pair-mode 1)

;; show keystrokes right away
(setq echo-keystrokes 0.1)

;; no startup messages
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message nil)

;; no start up screen
(setq inhibit-startup-screen t)

;; hide cursor in inactive windows
(setq cursor-in-non-selected-windows t)

;; make scratch buffer empty
(setq initial-scratch-message nil)

;; open scratch buffer in text mode
(setq initial-major-mode 'text-mode)

;; sentence end after one space line
(setq sentence-end-double-space nil)

;; ask before quit emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; select help window automatically, so it is easier to close it with `q`
(setq help-window-select t)

;; force ask y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; toggle truncate lines
(global-set-key (kbd "C-c e t") 'toggle-truncate-lines)

;; treat Esc like C-g
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; move file to trash when deleted
(setq-default delete-by-moving-to-trash t)



;;
;; Whitespace
;; ----------------------------------------------------------------------------
;;

;; show trailing whitespace in prog-mode
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; hide trailing whiespace in minibuffer
(add-hook 'minibuffer-inactive-mode-hook (lambda () (setq show-trailing-whitespace nil)))

;; add newline at EOF
(setq require-final-newline t)

;; delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; delete whitespaces more aggresively
(use-package hungry-delete
  :delight
  :config
  ;; don't delete newline
  (setq hungry-delete-chars-to-skip " \t\f\v")
  (global-hungry-delete-mode))



;;
;; Indentations
;; ----------------------------------------------------------------------------
;;

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

(global-set-key (kbd "<C-S-return>") 'rangi-insert-newline-above)
(global-set-key (kbd "<C-return>") 'rangi-insert-newline-below)

;; always indent with space
(setq-default indent-tabs-mode nil)

;; indent 2 space by default
(setq-default tab-width 2)

;; try to complete thing-at-point after current line is indented
(setq tab-always-indent 'complete)

;; indent automatically
(electric-indent-mode 1)



;;
;; Selection
;; ----------------------------------------------------------------------------
;;

;; remove selected text when inserting new text
(delete-selection-mode 1)

;; expand-region
(use-package expand-region
  :bind
  (("s-'" . er/expand-region)
   ("s-\"" . er/contract-region)))


;; randomize region
(defun rangi-randomize-region (beg end)
  "Randomize lines in region from BEG to END."
  (interactive "*r")
  (let ((lines (split-string
                (delete-and-extract-region beg end) "\n")))
    (when (string-equal "" (car (last lines 1)))
      (setq lines (butlast lines 1)))
    (apply 'insert
           (mapcar 'cdr
                   (sort (mapcar (lambda (x) (cons (random) (concat x "\n"))) lines)
                         (lambda (a b) (< (car a) (car b))))))))


;;
;; Cursors
;; ----------------------------------------------------------------------------
;;

(use-package multiple-cursors
  :init
  ;; save mc file in autogen directory
  (setq-default mc/list-file (expand-file-name "mc-lists.el" rangi-generated-files-directory))

  :config
  (global-set-key (kbd "s-d") 'mc/mark-next-like-this)
  (global-set-key (kbd "s-D") 'mc/mark-all-dwim)
  (global-set-key (kbd "M-s-d") 'mc/edit-lines))



;;
;; Navigation
;; ----------------------------------------------------------------------------
;;

;; enable subword mode
(use-package subword
  :delight
  :config
  (global-subword-mode))

;; move where I mean
(use-package mwim
  :bind
  (("C-a" . mwim-beginning-of-code-or-line)
   ("C-e" . mwim-end-of-code-or-line)))

;; move cursor to top or bottom of the buffer when it cannot be scrolled anymore
(setq-default scroll-error-top-bottom t)

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
(use-package avy
  :config
  (setq avy-background t)

  ;; use this function to active jump
  (defun rangi-active-cursor-jump (arg)
    (interactive "p")
    (message "Jump: (g): character (w): word (l): line (L): line in view")
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "g") 'avy-goto-char-timer)
       (define-key map (kbd "w") 'avy-goto-word-or-subword-1)
       (define-key map (kbd "l") 'goto-line)
       (define-key map (kbd "L") 'avy-goto-line)
       map)
     t))

  (bind-key "s-g" 'avy-goto-char-timer)
  (bind-key "M-g c" 'avy-goto-char-timer)
  (bind-key "s-G" 'rangi-active-cursor-jump))



;;
;; Clipboard & Kill Ring
;; ----------------------------------------------------------------------------
;;

;; allow us to select kill ring content from a list
(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))

;; ;; make stuff in system clipboard always saved in kill ring
;; (setq-default save-interprogram-paste-before-kill t)

;; separate system clipboard and kill ring
(use-package simpleclip
  :config
  (simpleclip-mode 1)

  (defun rangi-simpleclip-copy (beg end)
    "Call `simpleclip-copy', then deactive mark."
    (interactive "r")
    (call-interactively 'simpleclip-copy)
    (deactivate-mark))

  (bind-key "s-c" 'rangi-simpleclip-copy simpleclip-mode-map)
  (bind-key "C-<insert>" 'rangi-simpleclip-copy simpleclip-mode-map))


;; home-made package tp duplicate lines
(use-package duplicator
  :load-path "site-lisp/duplicator"
  :bind ("M-D" . duplicator/duplicate-lines))


;;
;; Undo & Redo
;; ----------------------------------------------------------------------------
;;

(use-package undo-tree
  :delight
  :bind (("s-z" . undo-tree-undo)
         ("s-Z" . undo-tree-redo))
  :config
  ;; show time differences in visualizer
  (setq undo-tree-visualizer-timestamps t)
  ;; show diff between changes in visualizer
  (setq undo-tree-visualizer-diff t)

  ;; store undo histories
  (setq undo-tree-auto-save-history t)
  (let ((dir (expand-file-name "undo" rangi-generated-files-directory)))
    (unless (file-exists-p dir) (make-directory dir))
    (setq undo-tree-history-directory-alist `(("." . ,dir))))
  (global-undo-tree-mode))


;;
;; Editing actions
;; ----------------------------------------------------------------------------
;;

;; zap
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;; comment smartly
(global-set-key (kbd "C-/") 'comment-dwim)

;; change case smartly
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

;; move text
(use-package move-text
  :config
  (move-text-default-bindings))

;; regex replace with visual guide
(use-package visual-regexp
  :config
  (bind-key "C-S-s-d" 'vr/mc-mark)
  (bind-key "M-%" 'vr/query-replace)
  (bind-key "s-r" 'vr/query-replace))



;;
;; Recent files
;; ----------------------------------------------------------------------------
;;

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" rangi-generated-files-directory))
  ;; avoid accidentally access remote file during cleanup
  (setq recentf-auto-cleanup 'never)
  ;; only store 50 items
  (setq recentf-max-menu-items 50)
  ;; save recent files every 5 mins, and do it silently
  (run-at-time nil (* 5 60)
               (lambda ()
                 (let ((save-silently t))
                   (recentf-save-list))))
  (recentf-mode 1))





(provide 'init-editor)
