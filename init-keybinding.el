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
;; add additonal key to works with multiple-cursor
(global-set-key (kbd "<M-return>") 'newline-and-indent)

;; expand region
(global-set-key (kbd "C-'") 'er/expand-region)
(global-set-key (kbd "C-\"") 'er/contract-region)

;; multiple-cursor
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-c C->") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; -------------------------------------------------- Text manipulation
;; kill line back
(global-set-key (kbd "C-S-k") 'kill-back-to-indentation)

;; better zap to char
(global-set-key (kbd "M-Z") 'zap-to-char)
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

;; move text
(global-set-key (kbd "<C-S-up>") 'move-text-up)
(global-set-key (kbd "<C-S-down>") 'move-text-down)


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

;; use ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; -------------------------------------------------- Others
(global-set-key (kbd "M-x") 'smex)

;;-------------------------------------------------- mode speicifed key
;; Ruby
(add-hook 'ruby-mode-hook
  (lambda()
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     (define-key ruby-mode-map (kbd "TAB") 'indent-for-tab-command)))

;; comint mode
(add-hook 'comint-mode-hook
  (lambda()
     (define-key comint-mode-map (kbd "<C-right>") 'windmove-right)
     (define-key comint-mode-map (kbd "<C-left>") 'windmove-left)
     (define-key comint-mode-map (kbd "<C-up>") 'windmove-up)
     (define-key comint-mode-map (kbd "<C-down>") 'windmove-down)))

(provide 'init-keybinding)
