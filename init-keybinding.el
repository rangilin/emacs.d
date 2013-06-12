;; -------------------------------------------------- Cursor movements
;; by character
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-j") 'backward-char)

;; by line
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "M-i") 'previous-line)

;; by word
(global-set-key (kbd "M-o") 'forward-word)
(global-set-key (kbd "M-u") 'backward-word)

;; by whitespace
(global-set-key (kbd "M-O") 'forward-whitespace)
(global-set-key (kbd "M-U") (lambda() (interactive) (forward-whitespace -1)))

;; by block
(global-set-key (kbd "M-I") 'previous-block)
(global-set-key (kbd "M-K") 'next-block)

;; page up/down
(global-set-key (kbd "M-<") 'scroll-cursor-up)
(global-set-key (kbd "M->") 'scroll-cursor-down)

;; to top/bottom of buffer
(global-set-key (kbd "M-Y") 'beginning-of-buffer)
(global-set-key (kbd "M-y") 'end-of-buffer)

;; to beginning/end of line
(global-set-key (kbd "M-H") 'back-to-indentation-or-beginning-of-line)
(global-set-key (kbd "M-h") 'move-end-of-line)

;; goto line
(global-set-key (kbd "C-c g") 'goto-line)

;; -------------------------------------------------- Editing
;; delete by character
(global-set-key (kbd "M-d") 'delete-backward-char)
(global-set-key (kbd "M-f") 'delete-char)

;; delete by word
(global-set-key (kbd "M-e") 'backward-kill-word)
(global-set-key (kbd "M-r") 'kill-word)

;; delete line
(global-set-key (kbd "M-g") 'kill-line)
(global-set-key (kbd "M-G") 'kill-back-to-indentation)
(global-set-key (kbd "C-S-g") 'kill-whole-line)

;; zap to character
(global-set-key (kbd "M-z") 'zap-to-char)

;; insert newline
(global-set-key (kbd "<M-S-return>") 'insert-newline-above)
(global-set-key (kbd "<S-return>") 'insert-newline-below)

;; copy, cut & paste
(global-set-key (kbd "M-x") 'kill-region)
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "M-V") 'yank-pop)

;; undo/redo
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)

;; move-text
(define-key input-decode-map (kbd "C-S-i") (kbd "H-S-i"))
(global-set-key (kbd "H-S-i") 'move-text-up)
(global-set-key (kbd "C-S-k") 'move-text-down)

;; join line
(global-set-key (kbd "C-j") (lambda () (interactive) (join-line 1)))
(global-set-key (kbd "C-J") 'join-line)

;; newline & indent
(global-set-key (kbd "RET") 'newline-and-indent)

;; toggle word case
(global-set-key (kbd "C-M-c") `toggle-letter-case)

;;---------------------------------------------------------------------------
;; Mark
;;----------------------------------------------------------------------------

;; Mark
(global-set-key (kbd "M-.") 'set-mark-command)

;; expand region
(global-set-key (kbd "M-w") 'er/expand-region)

;;----------------------------------------------------------------------------
;; Others
;;----------------------------------------------------------------------------
;; save
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-S-s") 'save-some-buffers)
;; search
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-S-f") 'isearch-backward)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-S-f") 'isearch-repeat-backward)

;; recenter
(global-set-key (kbd "M-p") 'recenter-top-bottom)

;; use smex to execute extend command
(global-set-key (kbd "M-a") 'smex)

;; use ibuufer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; window layout arragnement
(global-set-key (kbd "C-x 2") (focus-and-show-other-buffer-after 'split-window-vertically))
(global-set-key (kbd "C-x 3") (focus-and-show-other-buffer-after 'split-window-horizontally))
(global-set-key (kbd "C-x _") 'split-window-horizontally-instead)
(global-set-key (kbd "C-x |") 'split-window-vertically-instead)


(provide 'init-keybinding)

