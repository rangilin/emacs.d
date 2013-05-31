(global-set-key (kbd "C-j") 'join-line)

(global-set-key (kbd "C-S-j") (lambda () (interactive) (join-line 1)))

(global-set-key (kbd "M-x") 'smex)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "C-x 2") (focus-and-show-other-buffer-after 'split-window-vertically))
(global-set-key (kbd "C-x 3") (focus-and-show-other-buffer-after 'split-window-horizontally))
(global-set-key (kbd "C-x |") 'split-window-horizontally-instead)
(global-set-key (kbd "C-x _") 'split-window-vertically-instead)

(global-set-key (kbd "C-M-c") `toggle-letter-case)

(provide 'init-keybinding)
