;; -------------------------------------------------- Cursor movements
;; smarter move to beginning line
(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)

;; pop global mark
(global-set-key (kbd "C-`") 'pop-global-mark)

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

;; make mouse click can expand region like common editor
(global-set-key (kbd "<S-down-mouse-1>") 'mouse-save-then-kill)

;; multiple-cursor
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-c C->") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; -------------------------------------------------- Text manipulation
;; better zap to char
(global-set-key (kbd "M-Z") 'zap-to-char)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; browse kill ring
(global-set-key (kbd "C-S-y") 'browse-kill-ring)

;; duplicate thing
(global-set-key (kbd "C-c d") 'duplicate-thing)

;; join line
(global-set-key (kbd "C-j") (lambda () (interactive) (join-line 1)))
(global-set-key (kbd "C-S-j") 'join-line)

;; move text
(global-set-key (kbd "M-P") 'rangi-move-text-up)
(global-set-key (kbd "M-N") 'rangi-move-text-down)

;; -------------------------------------------------- Windows
;; switch window
(global-set-key (kbd "C-<tab>") 'other-window)

;; window layout arragnement
(global-set-key (kbd "C-x 2") (focus-and-show-other-buffer-after 'split-window-vertically))
(global-set-key (kbd "C-x 3") (focus-and-show-other-buffer-after 'split-window-horizontally))
(global-set-key (kbd "C-x _") 'split-window-horizontally-instead)
(global-set-key (kbd "C-x |") 'split-window-vertically-instead)

;; -------------------------------------------------- Buffer
;; switch buffer
(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "M-[") 'previous-buffer)

;; use ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; -------------------------------------------------- Others
(global-set-key (kbd "C-x C-m") 'smex)

;; Use shell-like backspace C-h
(define-key key-translation-map [?\C-h] [?\C-?])

(global-set-key (kbd "<f1>") 'help-command)

(global-set-key (kbd "C-z") 'shell)

(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)

;; loccur
(global-set-key (kbd "C-M-o") 'loccur-current)
(global-set-key (kbd "C-o") 'loccur)
(global-set-key (kbd "C-S-o") 'loccur-previous-match)

;; -------------------------------------------------- Unbind
(global-unset-key (kbd "C-x o"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "M-x"))
(global-unset-key (kbd "C-?"))

;;-------------------------------------------------- mode speicifed key
;; Ruby
(eval-after-load 'ruby-mode
  (lambda()
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     (define-key ruby-mode-map (kbd "TAB") 'indent-for-tab-command)))

;; undo tree mode
(eval-after-load 'undo-tree
  '(progn
     (define-key undo-tree-map (kbd "C-/") nil)
     (define-key undo-tree-map (kbd "C-?") nil)))

(provide 'init-keybinding)
