;;----------------------------------------------------------------------------
;; Movement
;;----------------------------------------------------------------------------
;; by line
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-k") 'next-line)

;; by character
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-j") 'backward-char)

;; by word
(global-set-key (kbd "M-o") 'forward-word)
(global-set-key (kbd "M-u") 'backward-word)

;; page up/down
(global-set-key (kbd "M-I") 'scroll-cursor-up)
(global-set-key (kbd "M-K") 'scroll-cursor-down)

;; to top/bottom of buffer
(global-set-key (kbd "M-Y") 'beginning-of-buffer)
(global-set-key (kbd "M-y") 'end-of-buffer)

;; to beginning/end of line
(global-set-key (kbd "M-J") 'move-beginning-of-line)
(global-set-key (kbd "M-L") 'move-end-of-line)

;; goto line
(global-set-key (kbd "C-c g") 'goto-line)

;;----------------------------------------------------------------------------
;; Deletion
;;----------------------------------------------------------------------------

;; by character
(global-set-key (kbd "M-d") 'delete-backward-char)
(global-set-key (kbd "M-f") 'delete-char)

;; by word
(global-set-key (kbd "M-e") 'backward-kill-word)
(global-set-key (kbd "M-r") 'kill-word)

;; kill line
(global-set-key (kbd "M-g") 'kill-line)
(global-set-key (kbd "M-G") 'kill-back-to-indentation)
(global-set-key (kbd "C-S-g") 'kill-whole-line)

;;----------------------------------------------------------------------------
;; Editing
;;----------------------------------------------------------------------------

;; insert newline
(global-set-key (kbd "<M-S-return>") 'insert-newline-above)
(global-set-key (kbd "<S-return>") 'insert-newline-below)

;; copy, cut & paste
(global-set-key (kbd "M-x") 'kill-region)
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "M-V") 'yank-pop)

;; undo/redo
(global-set-key (kbd "M-z") 'undo-tree-undo)
(global-set-key (kbd "M-Z") 'undo-tree-redo)

;;----------------------------------------------------------------------------
;; Others
;;----------------------------------------------------------------------------

;; Mark
(global-set-key (kbd "M-.") 'set-mark-command)

;; search
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-S-f") 'isearch-backward)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-S-f") 'isearch-repeat-backward)


;;----------------------------------------------------------------------------
(global-set-key (kbd "C-j") (lambda () (interactive) (join-line 1)))

(global-set-key (kbd "C-S-j") 'join-line)

(global-set-key (kbd "M-a") 'smex)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "C-x 2") (focus-and-show-other-buffer-after 'split-window-vertically))
(global-set-key (kbd "C-x 3") (focus-and-show-other-buffer-after 'split-window-horizontally))
(global-set-key (kbd "C-x |") 'split-window-horizontally-instead)
(global-set-key (kbd "C-x _") 'split-window-vertically-instead)

(global-set-key (kbd "C-M-c") `toggle-letter-case)


(provide 'init-keybinding)

