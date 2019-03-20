;;
;; Random stuff
;;----------------------------------------------------------------------------
;;

;; warn when open file larger than 100MB
(setq large-file-warning-threshold 100000000)

;; highlight matching parentheses
(show-paren-mode 1)

;; pairing parenthesis automatically
(electric-pair-mode 1)

;; show keystrokes right away
(setq echo-keystrokes 0.1)

;; no startup messages
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message nil)

;; no start up screen
(setq inhibit-startup-screen t)

;; hide cursor in inactive windows
(setq cursor-in-non-selected-windows t)

;; make scratch buffer empty
(setq initial-scratch-message nil)

;; open scratch buffer in text mode
(setq initial-major-mode 'text-mode)

;; sentence end after one space line
(setq sentence-end-double-space nil)

;; ask before quit emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; select help window automatically, so it is easier to close it with `q`
(setq help-window-select t)

;; force ask y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; remove selected text when inserting new text
(delete-selection-mode 1)

;; toggle truncate lines
(global-set-key (kbd "C-c e t") 'toggle-truncate-lines)

;; move cursor to top or bottom of the buffer when it cannot be scrolled anymore
(setq-default scroll-error-top-bottom t)



;;
;; Whitespace
;;----------------------------------------------------------------------------
;;


;; show trailing whitespace in prog-mode
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; hide trailing whiespace in minibuffer
(add-hook 'minibuffer-inactive-mode-hook (lambda () (setq show-trailing-whitespace nil)))


(provide 'init-editor)
