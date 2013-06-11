;;----------------------------------------------------------------------------
;; Movement
;;----------------------------------------------------------------------------
;; by line
(global-set-key previous-line-key 'previous-line)
(global-set-key next-line-key 'next-line)

;; by character
(global-set-key forward-char-key 'forward-char)
(global-set-key backward-char-key 'backward-char)

;; by word
(global-set-key forward-word-key 'forward-word)
(global-set-key backward-word-key 'backward-word)

;; by block
(global-set-key previous-block-key 'previous-block)
(global-set-key next-block-key 'next-block)

;; page up/down
(global-set-key scroll-cursor-up-key 'scroll-cursor-up)
(global-set-key scroll-cursor-down-key 'scroll-cursor-down)

;; to top/bottom of buffer
(global-set-key beginning-of-buffer-key 'beginning-of-buffer)
(global-set-key end-of-buffer-key 'end-of-buffer)

;; to beginning/end of line
(global-set-key back-to-indentation-or-beginning-of-line-key
                'back-to-indentation-or-beginning-of-line)
(global-set-key move-end-of-line-key 'move-end-of-line)

;; goto line
(global-set-key goto-line-key 'goto-line)

;;----------------------------------------------------------------------------
;; Deletion
;;----------------------------------------------------------------------------

;; by character
(global-set-key delete-backward-char-key 'delete-backward-char)
(global-set-key delete-forward-char-key 'delete-char)

;; by word
(global-set-key backward-kill-word-key 'backward-kill-word)
(global-set-key forward-kill-word-key 'kill-word)

;; kill line
(global-set-key kill-line-key 'kill-line)
(global-set-key kill-back-to-indentation-key 'kill-back-to-indentation)
(global-set-key kill-whole-line-key 'kill-whole-line)

;;----------------------------------------------------------------------------
;; Editing
;;----------------------------------------------------------------------------

;; insert newline
(global-set-key insert-newline-above-key 'insert-newline-above)
(global-set-key insert-newline-below-key 'insert-newline-below)

;; copy, cut & paste
(global-set-key kill-region-key 'kill-region)
(global-set-key kill-ring-save-key 'kill-ring-save)
(global-set-key yank-key 'yank)
(global-set-key yank-pop-key 'yank-pop)

;; undo/redo
(global-set-key undo-key 'undo-tree-undo)
(global-set-key redo-key 'undo-tree-redo)

;; move-text
(global-set-key move-text-up-key 'move-text-up)
(global-set-key move-text-down-key 'move-text-down)

;; save
(global-set-key save-buffer-key 'save-buffer)
(global-set-key save-some-buffers-key 'save-some-buffers)

;;----------------------------------------------------------------------------
;; Others
;;----------------------------------------------------------------------------

;; Mark
(global-set-key set-mark-command-key 'set-mark-command)

;; search
(global-set-key isearch-forward-key 'isearch-forward)
(global-set-key isearch-backward-key 'isearch-backward)
(define-key isearch-mode-map isearch-forward-key 'isearch-repeat-forward)
(define-key isearch-mode-map isearch-backward-key 'isearch-repeat-backward)

;; recenter
(global-set-key recenter-key 'recenter-top-bottom)

;;----------------------------------------------------------------------------
(global-set-key join-line-below-key (lambda () (interactive) (join-line 1)))
(global-set-key join-line-above-key 'join-line)

(global-set-key execute-extend-command-key 'smex)

(global-set-key ibuffer-key 'ibuffer)

(global-set-key newline-and-indent-key 'newline-and-indent)

(global-set-key split-window-vertically-key
                (focus-and-show-other-buffer-after 'split-window-vertically))
(global-set-key split-window-horizontally-key
                (focus-and-show-other-buffer-after 'split-window-horizontally))
(global-set-key split-window-horizontally-instead-key 'split-window-horizontally-instead)
(global-set-key split-window-vertically-instead-key 'split-window-vertically-instead)

(global-set-key toggle-word-letter-case-key `toggle-letter-case)


(provide 'init-keybinding)
