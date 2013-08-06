;; -------------------------------------------------- Cursor movements
;; smarter move to beginning line
(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)

;; jump to char
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-M") 'jump-char-backward)

;; better scrolling
(global-set-key (kbd "C-v") 'scroll-cursor-down)
(global-set-key (kbd "M-v") 'scroll-cursor-up)

;; insert newline
(global-set-key (kbd "<M-S-return>") 'insert-newline-above)
(global-set-key (kbd "<S-return>") 'insert-newline-below)

;; newline & indent
(global-set-key (kbd "RET") 'newline-and-indent)

;; expand region
(global-set-key (kbd "C-'") 'er/expand-region)
(global-set-key (kbd "C-\"") 'er/contract-region)

;; -------------------------------------------------- Text manipulation
;; kill line back
(global-set-key (kbd "C-S-k") 'kill-back-to-indentation)

;; better zap to char
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; browse kill ring
(global-set-key (kbd "M-y") 'browse-kill-ring)

;; duplicate thing
(global-set-key (kbd "C-c d") 'duplicate-thing)

;; undo/redo
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)

;; join line
(global-set-key (kbd "C-j") (lambda () (interactive) (join-line 1)))
(global-set-key (kbd "C-S-j") 'join-line)

;; -------------------------------------------------- Windows
;; switch window
(global-set-key (kbd "<C-right>") 'windmove-right)
(global-set-key (kbd "<C-left>") 'windmove-left)
(global-set-key (kbd "<C-up>") 'windmove-up)
(global-set-key (kbd "<C-down>") 'windmove-down)

;; window layout arragnement
(global-set-key (kbd "C-x 2") (focus-and-show-other-buffer-after 'split-window-vertically))
(global-set-key (kbd "C-x 3") (focus-and-show-other-buffer-after 'split-window-horizontally))
(global-set-key (kbd "C-x _") 'split-window-horizontally-instead)
(global-set-key (kbd "C-x |") 'split-window-vertically-instead)

;; -------------------------------------------------- Buffer
;; switch buffer
(global-set-key (kbd "<M-right>") 'next-buffer)
(global-set-key (kbd "<M-left>") 'previous-buffer)

;; -------------------------------------------------- Others
(global-set-key (kbd "M-x") 'smex)

;;-------------------------------------------------- mode speicifed key
;; Ruby
(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     (define-key ruby-mode-map (kbd "TAB") 'indent-for-tab-command)))



(provide 'init-keybinding)
