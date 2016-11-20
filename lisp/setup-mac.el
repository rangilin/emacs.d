;; Mac specified setup

(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

(when window-system
  (set-frame-size (selected-frame) 120 42)
  (set-frame-position (selected-frame) 140 25))

(provide 'setup-mac)
