;; change modifiers

;; command keys are `super'
(setq mac-command-modifier 'super)
(setq mac-right-command-modifier 'super)

;; option keys are `meta'
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'meta)

;; add MacOS like keys
(bind-key "s-k" 'kill-current-buffer)
(bind-key "s-a" 'mark-whole-buffer)
(bind-key "s-v" 'yank)
(bind-key "s-c" 'copy-region-as-kill)
(bind-key "s-x" 'kill-region)


(provide 'init-macos)
