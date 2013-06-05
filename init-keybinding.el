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
;; Others
;;----------------------------------------------------------------------------

;; Mark
(global-set-key (kbd "M-.") 'set-mark-command)

;; cancel
(global-set-key (kbd "M-n") 'keyboard-quit)
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-j") (lambda () (interactive) (join-line 1)))

(global-set-key (kbd "C-S-j") 'join-line)

(global-set-key (kbd "M-x") 'smex)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "C-x 2") (focus-and-show-other-buffer-after 'split-window-vertically))
(global-set-key (kbd "C-x 3") (focus-and-show-other-buffer-after 'split-window-horizontally))
(global-set-key (kbd "C-x |") 'split-window-horizontally-instead)
(global-set-key (kbd "C-x _") 'split-window-vertically-instead)

(global-set-key (kbd "C-M-c") `toggle-letter-case)


(provide 'init-keybinding)

