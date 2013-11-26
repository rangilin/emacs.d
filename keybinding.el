(require 'use-package)
;; -------------------------------------------------- Cursor movements
;; smarter move to beginning line
(bind-key "C-a" 'smarter-move-beginning-of-line)

;; better scrolling
(bind-key "C-v" 'scroll-cursor-down)
(bind-key "M-v" 'scroll-cursor-up)

;; insert newline
(bind-key "<M-S-return>" 'insert-newline-above)
(bind-key "<S-return>" 'insert-newline-below)

;; newline & indent
(bind-key "RET" 'newline-and-indent)
(bind-key "<M-return>" 'newline-and-indent)

;; make mouse click can expand region like common editor
(bind-key "<S-down-mouse-1>" 'mouse-save-then-kill)

;; horizontal recenter
(bind-key "C-S-l" 'rl/horizontal-recenter)

;; -------------------------------------------------- Text manipulation
;; backward kill
(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
(bind-key "M-h" 'backward-kill-word)

;; kill-whole-line
(define-key key-translation-map (kbd "M-H") (kbd "<C-S-backspace>"))

;; comment
(bind-key "C-/" 'comment-or-uncomment-region-or-line)

;; join line
(bind-key "C-j" (lambda () (interactive) (join-line 1)))
(bind-key "C-S-j" 'join-line)

;; -------------------------------------------------- Windows
;; switch window
(bind-key "M-o" 'other-window)
(bind-key "M-O" 'rl/previous-window)

;; window layout management
(bind-key "C-x 2" (focus-and-show-other-buffer-after 'split-window-vertically))
(bind-key "C-x 3" (focus-and-show-other-buffer-after 'split-window-horizontally))
(bind-key "C-x _" 'split-window-horizontally-instead)
(bind-key "C-x |" 'split-window-vertically-instead)

;; -------------------------------------------------- Buffers
;; switch buffer
(bind-key "M-]" 'next-buffer)
(bind-key "M-[" 'previous-buffer)

;;-------------------------------------------------- unbind
(global-unset-key (kbd "C-x o"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "M-x"))
(global-unset-key (kbd "C-?"))
(global-unset-key (kbd "C-z"))




(provide 'keybinding)
