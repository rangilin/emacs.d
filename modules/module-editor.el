;; -------------------------------------------------------------------
;;
;; #cursor
;;
;; -------------------------------------------------------------------


(defun rl-multiple-cursors (arg)
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


(use-package multiple-cursors
  :ensure t
  :bind ("C->" . rl-multiple-cursors)
  :init (setq-default mc/list-file (expand-file-name ".mc-lists.el" (file-name-as-directory (expand-file-name "autogen" user-emacs-directory)))))


(use-package subword
  :diminish ""
  :config
  (global-subword-mode))


;; -------------------------------------------------------------------
;;
;; #selection
;;
;; -------------------------------------------------------------------


(defun rl-mark-line (&optional arg)
  "Mark a line."
  (interactive "p")
  (beginning-of-line)
  (let ((here (point)))
    (dotimes (i arg)
      (end-of-line))
    (set-mark (point))
    (goto-char here)))


(defun rl-mark-sentence (&optional arg)
  "Mark a sentence."
  (interactive "P")
  (backward-sentence)
  (mark-end-of-sentence arg))


;; Make marked characters will be replaced when changed.
(delete-selection-mode 1)


;; Set up some marking keybindings.
(bind-key "M-L" 'rl-mark-line)
(bind-key "M-S" 'rl-mark-sentence)
(bind-key "M-X" 'mark-sexp)
(bind-key "M-D" 'mark-defun)


(use-package expand-region
  :ensure t
  :bind ("C-'" . er/expand-region))


;; -------------------------------------------------------------------
;;
;; #yasnippet
;;
;; -------------------------------------------------------------------


(require 'yasnippet)

(defun rl-yas-ivy-prompt (prompt choices &optional display-fn)
  (yas-completing-prompt prompt choices display-fn #'ivy-completing-read))


(use-package yasnippet
  :diminish yas-minor-mode
  :ensure t
  :config
  (setq yas-snippet-dirs (concat (file-name-as-directory user-emacs-directory) "snippets"))
  (yas-global-mode 1))


;; -------------------------------------------------------------------
;;
;; #undo/redo
;;
;; -------------------------------------------------------------------


(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1)
  (bind-key "C-/" nil undo-tree-map)
  (bind-key "C-?" nil undo-tree-map))


;; -------------------------------------------------------------------
;;
;; #navigation
;;
;; -------------------------------------------------------------------


;; http://www.emacswiki.org/emacs/BackToIndentationOrBeginning
(defun rl-back-to-indentation-or-beginning ()
  "Back to indentation or beginning of current line"
  (interactive "^")
  (if (bound-and-true-p visual-line-mode)
      (beginning-of-visual-line)
    (when (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line))))

;; http://stackoverflow.com/a/1249665/554279
(defun rl-horizontal-recenter ()
  "make the point horizontally centered in the window"
  (interactive)
  (let ((mid (/ (window-width) 2))
        (line-len (save-excursion (end-of-line) (current-column)))
        (cur (current-column)))
    (if (< mid cur) (set-window-hscroll (selected-window) (- cur mid)))))


(use-package ace-window
  :ensure t
  :init
  (setq-default aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package avy
  :ensure t
  :init
  (setq avy-background t))

;; move cursor to top or bottom when it can not scroll
(setq-default scroll-error-top-bottom t)

(bind-key "C-S-l" 'rl-horizontal-recenter)
(bind-key "C-a" 'rl-back-to-indentation-or-beginning)


;; -------------------------------------------------------------------
;;
;; #pairs
;;
;; -------------------------------------------------------------------


(show-paren-mode 1)
(electric-pair-mode 1)


;; -------------------------------------------------------------------
;;
;; #indentation
;;
;; -------------------------------------------------------------------


(defun rl-insert-newline-above ()
  "Insert a newline above the current line."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))


(defun rl-insert-newline-below ()
  "Insert a newline below the current line."
  (interactive)
  (end-of-line)
  (newline)
  (indent-according-to-mode))


(bind-key "<M-S-return>" 'rl-insert-newline-above)
(bind-key "<S-return>" 'rl-insert-newline-below)


(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default tab-always-indent 'complete)

;; -------------------------------------------------------------------
;;
;; #whitespace
;;
;; -------------------------------------------------------------------


(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :config
  (global-hungry-delete-mode))


;; show trailing whitespace in following mode
(defun rl-show-trailing-whitespace ()
  (setq show-trailing-whitespace t))
(add-hook 'prog-mode-hook 'rl-show-trailing-whitespace)


;; hide trailing whitespace in following mode
(defun rl-hide-trailing-whitespace ()
  (setq show-trailing-whitespace nil))
(add-hook 'minibuffer-inactive-mode-hook 'rl-hide-trailing-whitespace)


;; clean trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; -------------------------------------------------------------------
;;
;; kill ring
;;
;; -------------------------------------------------------------------


(use-package browse-kill-ring
  :ensure t
  :config
  (browse-kill-ring-default-keybindings))


;; -------------------------------------------------------------------
;;
;; bookmark
;;
;; -------------------------------------------------------------------


(use-package bookmark
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" (file-name-as-directory (expand-file-name "autogen" user-emacs-directory)))))


(provide 'module-editor)
