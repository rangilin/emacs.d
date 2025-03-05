;;; init-macos.el --- MacOS related configurations -*- lexical-binding: t -*-

;; set command keys as `super'
(setq mac-command-modifier 'super)
(setq mac-right-command-modifier 'super)

;; set option keys as `meta'
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'meta)

;; add back some common MacOS keys that feels Emacs behaviors
(bind-key "s-w" 'kill-current-buffer)
(bind-key "s-a" 'mark-whole-buffer)
(bind-key "s-v" 'yank)
(bind-key "s-c" 'copy-region-as-kill)
(bind-key "s-x" 'kill-region)

;; use `mdfind' on MacOS instead of `locate'
(setq-default locate-command "mdfind")

(provide 'init-macos)
