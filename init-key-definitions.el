;; -------------------------------------------------- Cursor movements
;; move by chracter
(defconst rl-forward-char-key (kbd "M-l"))
(defconst rl-backward-char-key (kbd "M-j"))

;; move by word
(defconst rl-forward-word-key (kbd "M-o"))
(defconst rl-backward-word-key (kbd "M-u"))

;; move by line
(defconst rl-forward-line-key (kbd "M-k"))
(defconst rl-backward-line-key (kbd "M-i"))

;; move by text block
(defconst rl-backward-block-key (kbd "M-U"))
(defconst rl-forward-block-key (kbd "M-O"))

;; move by page
(defconst rl-backward-page-key (kbd "M-I"))
(defconst rl-forward-page-key (kbd "M-K"))

;; move to buffer top/bottom
(defconst rl-beginning-of-buffer-key (kbd "M-Y"))
(defconst rl-end-of-buffer-key (kbd "M-y"))

;; move to line beginning/end
(defconst rl-move-to-indentation-or-beginning-of-line-key (kbd "M-H")
  "Move to the indentation, move the the beginning of the line if repeated.")
(defconst rl-move-to-end-of-line-key (kbd "M-h"))

;; move to given line
(defconst rl-goto-line-key (kbd "C-c g"))

;; -------------------------------------------------- Editing
;; delete by character
(defconst rl-delete-backward-char-key (kbd "M-d"))
(defconst rl-delete-forward-char-key (kbd "M-f"))

;; delete by word
(defconst rl-delete-backward-word-key (kbd "M-e"))
(defconst rl-delete-forward-word-key (kbd "M-r"))

;; delete line
(defconst rl-delete-to-end-of-line-key (kbd "M-g"))
(defconst rl-delete-to-indentation-of-line-key (kbd "M-G"))
(defconst rl-delete-whole-line-key (kbd "C-S-g"))

;; insert newline above/below
(defconst rl-insert-newline-above-key (kbd "<M-S-return>"))
(defconst rl-insert-newline-below-key (kbd "<S-return>"))

;; copy, paste, cut
(defconst rl-cut-key (kbd "M-x"))
(defconst rl-copy-key (kbd "M-c"))
(defconst rl-paste-key (kbd "M-v"))
(defconst rl-paste-history-key (kbd "M-V"))

;; undo, redo
(defconst rl-undo-key (kbd "M-z"))
(defconst rl-redo-key (kbd "M-Z"))

;; move text
(defconst rl-move-text-up-key (kbd "H-S-i"))
(define-key input-decode-map (kbd "C-S-i") rl-move-text-up-key)
(defconst rl-move-text-down-key (kbd "C-S-k"))

;; join lines
(defconst rl-join-line-below-key (kbd "C-j"))
(defconst rl-join-line-above-key (kbd "C-S-j"))

;; insert newline and indent
(defconst rl-newline-and-indent-key (kbd "RET"))

;; toogle letter case of a word
(defconst rl-toggle-word-letter-case-key (kbd "C-M-c"))

;; -------------------------------------------------- Others
;; save buffers
(defconst rl-save-buffer-key (kbd "C-s"))
(defconst rl-save-some-buffers-key (kbd "C-S-s"))

;; mark
(defconst rl-set-mark-command-key (kbd "M-."))

;; increment search
(defconst rl-isearch-forward-key (kbd "C-f"))
(defconst rl-isearch-backward-key (kbd "C-S-f"))

;; recenter
(defconst rl-recenter-key (kbd "M-p"))

;; execute extend command
(defconst rl-execute-extend-command-key (kbd "M-a"))

;; ibuffer
(defconst rl-ibuffer-key (kbd "C-x C-b"))

;; window layout arrangement
(defconst rl-split-window-vertically-key (kbd "C-x 2"))
(defconst rl-split-window-horizontally-key (kbd "C-x 3"))
(defconst rl-split-window-vertically-instead-key (kbd "C-x _"))
(defconst rl-split-window-horizontally-instead-key (kbd "C-x |"))


(provide 'init-key-definitions)

