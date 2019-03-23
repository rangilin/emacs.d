;; change modifiers
(setq-default mac-command-modifier 'meta)
(setq-default mac-option-modifier 'super)


;; reduce scrolling speed of mouse/trackpad
(setq mouse-wheel-scroll-amount '(1
				  ((shift) . 5)
				  ((control))))


;; make these key shortcut consistent with MacOS's native behaviors
(global-set-key (kbd "M-`") 'ns-next-frame)
(global-set-key (kbd "M-h") 'ns-do-hide-emacs)
(global-set-key (kbd "M-s-h") 'ns-do-hide-others)



(provide 'init-mac-os)
