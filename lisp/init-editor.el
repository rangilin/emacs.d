;;;; parenthese

;; highlight matching parentheses
(show-paren-mode 1)

;; pairing parenthesis automatically
(electric-pair-mode 1)

;; toggle truncate lines
(global-set-key (kbd "C-c e t") 'toggle-truncate-lines)


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

;; save mc file in autogen directory
(setq-default mc/list-file (expand-file-name "mc-lists.el" rangi-generated-files-directory))

(require 'multiple-cursors)

;; ;; use this function to active multiple cursor mode
(defun rangi-active-multiple-cursors (arg)
  (interactive "p")
  (message "Multiple cursors is activated...")
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd ">") 'mc/mark-all-like-this)
     (define-key map (kbd "l") 'mc/edit-lines)
     (define-key map (kbd "r") 'mc/mark-all-in-region-regexp)

     (define-key map (kbd "<down-mouse-1>") 'ignore)
     (define-key map (kbd "<mouse-1>") 'mc/toggle-cursor-on-click)

     (define-key map (kbd "n") 'mc/mark-next-like-this)
     (define-key map (kbd "N") 'mc/skip-to-next-like-this)
     (define-key map (kbd "M-n") 'mc/unmark-next-like-this)

     (define-key map (kbd "p") 'mc/mark-previous-like-this)
     (define-key map (kbd "P") 'mc/skip-to-previous-like-this)
     (define-key map (kbd "M-p") 'mc/unmark-previous-like-this)

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

;; move where I mean
(require-package 'mwim)
(global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
(global-set-key (kbd "C-e") 'mwim-end-of-code-or-line)


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
  (message "Jump: (g):word, (G):character, (l):line, (L):line in view")
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "g") 'avy-goto-word-1)
     (define-key map (kbd "G") 'avy-goto-char)
     (define-key map (kbd "l") 'goto-line)
     (define-key map (kbd "L") 'avy-goto-line)
   map)
   t))

(global-set-key (kbd "M-g") 'avy-goto-word-1)
(global-set-key (kbd "M-G") 'rangi-active-cursor-jump)




;;;; Whitespaces
;; show trailing whitespace in prog-mode
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; hide trailing whiespace in minibuffer
(add-hook 'minibuffer-inactive-mode-hook (lambda () (setq show-trailing-whitespace nil)))

;; delete whitespaces more aggresively
(require-package 'hungry-delete)
(require 'hungry-delete)
(setq hungry-delete-chars-to-skip " \t\f\v") ; but don't delete newline

(global-hungry-delete-mode)
(diminish 'hungry-delete-mode)



;;;; Clean & Indent
(require-package 'clean-aindent-mode)
(require 'clean-aindent-mode)

;; disable other indent mode
(electric-indent-mode -1)

;; clean whitespace smartly
(clean-aindent-mode t)

;; use simple indent
(setq clean-aindent-is-simple-indent t)

(define-key global-map (kbd "RET") 'newline-and-indent)




;;;; Yank & Kill
(require-package 'browse-kill-ring)
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)




;;;; Undo & Redo
(require-package 'undo-tree)
(require 'undo-tree)
(add-hook 'after-init-hook 'global-undo-tree-mode)
(with-eval-after-load 'undo-tree
  (define-key undo-tree-map (kbd "C-/") nil)
  (diminish 'undo-tree-mode))




;;;; Move stuff
(require-package 'move-text)
(require 'move-text)
(global-set-key (kbd "M-P") 'move-text-up)
(global-set-key (kbd "M-N") 'move-text-down)




;;;; zap
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)




;;;; copy/duplicate stuff
(require 'duplicator)
(global-set-key (kbd "M-D") 'duplicator/duplicate-lines)


;; make sure stuff in clipboard always saved in kill ring
(setq-default save-interprogram-paste-before-kill t)


(defun rangi-copy-current-path (&optional prefix)
  "Copy file path of the current buffer"
  (interactive "p")
  (let ((path (rangi-current-path)))
    (if path
        (progn
          (let ((result (rangi-strip-path (/ prefix 4) path)))
            (kill-new result)
            (message "Copied '%s' to the clipboard." result)))
      (message "Not in a file or directory, do nothing"))))


(defun rangi-current-path ()
  "Return full path of current buffer"
  (if (equal major-mode `dired-mode)
      default-directory
    (buffer-file-name)))

(defun rangi-strip-path (index path)
  "Strip path according to index, 1 will return last element of the path,
4 return parent directory, otherwise return path itself"
  (cond
   ((equal 1 index) (rangi-path-base path))
   ((equal 4 index) (file-name-directory (directory-file-name path)))
   (t path)))

(defun rangi-path-base (path)
  "Return last element of path of PATH"
  (let* ((parent-dir (file-name-directory (directory-file-name path))))
    (s-chop-prefix parent-dir (directory-file-name path))))


(global-set-key (kbd "C-c c f") 'rangi-copy-current-path)






;;;; randomize region
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



;; comment
(global-set-key (kbd "C-/") 'comment-dwim)




;; generate linear ranges
(require-package 'tiny)
(require 'tiny)
(tiny-setup-default)


(provide 'init-editor)
