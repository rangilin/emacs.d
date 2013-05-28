;;;; Keybinding goes here

(global-set-key (kbd "C-j") 'join-line)
ma
(global-set-key (kbd "C-S-j") (lambda () (interactive) (join-line 1)))

(global-set-key (kbd "M-x") 'smex)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "RET") 'newline-and-indent)

(provide 'init-keybinding)
