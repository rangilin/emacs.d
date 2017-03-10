;; Turn interfaces off.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)



;; Turn splash screen off.
(setq inhibit-startup-message t)



;; Blink the modeline on errors.
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.05 nil 'invert-face 'mode-line)))



;; Highlight current line.
(global-hl-line-mode 1)



;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)



;; Use y/n to confirm dialog.
(fset 'yes-or-no-p 'y-or-n-p)



;; Keep cursor position while scrolling.
(setq scroll-conservatively 10000
      scroll-preserve-screen-position t)



;; Don't print initial message in scratch buffer.
(setq initial-scratch-message "")



;; Load spacemacs theme. I use dark theme by default.
(use-package spacemacs-theme
  :ensure t)
(load-theme 'spacemacs-dark t)



(provide 'module-gui)
