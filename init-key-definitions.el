;; All key definitions

;; -------------------------------------------------- Cursor movements
;; move by chracter
(defconst forward-char-key (kbd "M-l"))
(defconst backward-char-key (kbd "M-j"))

;; move by word
(defconst forward-word-key (kbd "M-o"))
(defconst backward-word-key (kbd "M-u"))

;; move by line
(defconst previous-line-key (kbd "M-i"))
(defconst next-line-key (kbd "M-k"))

;; move by text block
(defconst previous-block-key (kbd "M-U"))
(defconst next-block-key (kbd "M-O"))

;; move by page
(defconst scroll-cursor-up-key (kbd "M-I"))
(defconst scroll-cursor-down-key (kbd "M-K"))

;; move to buffer top/bottom
(defconst beginning-of-buffer-key (kbd "M-Y"))
(defconst end-of-buffer-key (kbd "M-y"))

;; move to line beginning/end
(defconst back-to-indentation-or-beginning-of-line-key (kbd "M-H")
  "Move to the indentation, move the the beginning of the line if repeated.")
(defconst move-end-of-line-key (kbd "M-h"))

;; move to given line
(defconst goto-line-key (kbd "C-c g"))

;; -------------------------------------------------- Editing
;; delete by character
(defconst delete-backward-char-key (kbd "M-d"))
(defconst delete-forward-char-key (kbd "M-f"))

;; delete by word
(defconst backward-kill-word-key (kbd "M-e"))
(defconst forward-kill-word-key (kbd "M-r"))

;; delete line
(defconst kill-line-key (kbd "M-g"))
(defconst kill-back-to-indentation-key (kbd "M-G"))
(defconst kill-whole-line-key (kbd "C-S-g"))

;; insert newline above/below
(defconst insert-newline-above-key (kbd "<M-S-return>"))
(defconst insert-newline-below-key (kbd "<S-return>"))

;; copy, paste, cut
(defconst kill-region-key (kbd "M-x"))
(defconst kill-ring-save-key (kbd "M-c"))
(defconst yank-key (kbd "M-v"))
(defconst yank-pop-key (kbd "M-V"))

;; undo, redo
(defconst undo-key (kbd "M-z"))
(defconst redo-key (kbd "M-Z"))

;; move text
(defconst move-text-up-key (kbd "H-S-i"))
(define-key input-decode-map (kbd "C-S-i") move-text-up-key)
(defconst move-text-down-key (kbd "C-S-k"))

;; join lines
(defconst join-line-below-key (kbd "C-j"))
(defconst join-line-above-key (kbd "C-S-j"))

;; insert newline and indent
(defconst newline-and-indent-key (kbd "RET"))

;; toogle letter case of a word
(defconst toggle-word-letter-case-key (kbd "C-M-c"))

;; -------------------------------------------------- Others
;; save buffers
(defconst save-buffer-key (kbd "C-s"))
(defconst save-some-buffers-key (kbd "C-S-s"))

;; mark
(defconst set-mark-command-key (kbd "M-."))

;; increment search
(defconst isearch-forward-key (kbd "C-f"))
(defconst isearch-backward-key (kbd "C-S-f"))

;; recenter
(defconst recenter-key (kbd "M-p"))

;; execute extend command
(defconst execute-extend-command-key (kbd "M-a"))

;; ibuffer
(defconst ibuffer-key (kbd "C-x C-b"))

;; window layout arrange
(defconst split-window-vertically-key (kbd "C-x 2"))
(defconst split-window-horizontally-key (kbd "C-x 3"))
(defconst split-window-vertically-instead-key (kbd "C-x _"))
(defconst split-window-horizontally-instead-key (kbd "C-x |"))


(provide 'init-key-definitions)

