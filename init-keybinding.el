;; -------------------------------------------------- Cursor movements
;; by line
(global-set-key rl-forward-line-key 'next-line)
(global-set-key rl-backward-line-key 'previous-line)

;; by character
(global-set-key rl-forward-char-key 'forward-char)
(global-set-key rl-backward-char-key 'backward-char)

;; by word
(global-set-key rl-forward-word-key 'forward-word)
(global-set-key rl-backward-word-key 'backward-word)

;; by block
(global-set-key rl-forward-block-key 'next-block)
(global-set-key rl-backward-block-key 'previous-block)

;; page up/down
(global-set-key rl-forward-page-key 'scroll-cursor-down)
(global-set-key rl-backward-page-key 'scroll-cursor-up)

;; to top/bottom of buffer
(global-set-key rl-beginning-of-buffer-key 'beginning-of-buffer)
(global-set-key rl-end-of-buffer-key 'end-of-buffer)

;; to beginning/end of line
(global-set-key rl-move-to-indentation-or-beginning-of-line-key
                'back-to-indentation-or-beginning-of-line)
(global-set-key rl-move-to-end-of-line-key 'move-end-of-line)

;; goto line
(global-set-key rl-goto-line-key 'goto-line)

;; -------------------------------------------------- Editing
;; delete by character
(global-set-key rl-delete-backward-char-key 'delete-backward-char)
(global-set-key rl-delete-forward-char-key 'delete-char)

;; delete by word
(global-set-key rl-delete-backward-word-key 'backward-kill-word)
(global-set-key rl-delete-forward-word-key 'kill-word)

;; delete line
(global-set-key rl-delete-to-end-of-line-key 'kill-line)
(global-set-key rl-delete-to-indentation-of-line-key 'kill-back-to-indentation)
(global-set-key rl-delete-whole-line-key 'kill-whole-line)

;; insert newline
(global-set-key rl-insert-newline-above-key 'insert-newline-above)
(global-set-key rl-insert-newline-below-key 'insert-newline-below)

;; copy, cut & paste
(global-set-key rl-cut-key 'kill-region)
(global-set-key rl-copy-key 'kill-ring-save)
(global-set-key rl-paste-key 'yank)
(global-set-key rl-paste-history-key 'yank-pop)

;; undo/redo
(global-set-key rl-undo-key 'undo-tree-undo)
(global-set-key rl-redo-key 'undo-tree-redo)

;; move-text
(global-set-key rl-move-text-up-key 'move-text-up)
(global-set-key rl-move-text-down-key 'move-text-down)

;; join line
(global-set-key rl-join-line-below-key (lambda () (interactive) (join-line 1)))
(global-set-key rl-join-line-above-key 'join-line)

;; newline & indent
(global-set-key rl-newline-and-indent-key 'newline-and-indent)

;; toggle word case
(global-set-key rl-toggle-word-letter-case-key `toggle-letter-case)

;;---------------------------------------------------------------------------
;; Mark
;;----------------------------------------------------------------------------

;; Mark
(global-set-key rl-set-mark-command-key 'set-mark-command)

;; expand region
(global-set-key rl-expand-region-key 'er/expand-region)

;;----------------------------------------------------------------------------
;; Others
;;----------------------------------------------------------------------------
;; save
(global-set-key rl-save-buffer-key 'save-buffer)
(global-set-key rl-save-some-buffers-key 'save-some-buffers)
;; search
(global-set-key rl-isearch-forward-key 'isearch-forward)
(global-set-key rl-isearch-backward-key 'isearch-backward)
(define-key isearch-mode-map rl-isearch-forward-key 'isearch-repeat-forward)
(define-key isearch-mode-map rl-isearch-backward-key 'isearch-repeat-backward)

;; recenter
(global-set-key rl-recenter-key 'recenter-top-bottom)

;; use smex to execute extend command
(global-set-key rl-execute-extend-command-key 'smex)

;; use ibuufer
(global-set-key rl-ibuffer-key 'ibuffer)

;; window layout arragnement
(global-set-key rl-split-window-vertically-key
                (focus-and-show-other-buffer-after 'split-window-vertically))
(global-set-key rl-split-window-horizontally-key
                (focus-and-show-other-buffer-after 'split-window-horizontally))
(global-set-key rl-split-window-horizontally-instead-key 'split-window-horizontally-instead)
(global-set-key rl-split-window-vertically-instead-key 'split-window-vertically-instead)



(provide 'init-keybinding)
