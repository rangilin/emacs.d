(require 'use-package)

;; ------------------------------ split window
(defun rl/split-window-vertically ()
  "Split window vertically. Focus to the new window and display a recent used
buffer in it"
  (interactive)
  (call-interactively 'split-window-vertically)
  (set-window-buffer (next-window) (other-buffer))
  (other-window 1))

(defun rl/split-window-horizontally ()
  "Split window horizontally. Focus to the new window and display a recent used
buffer in it"
  (interactive)
  (call-interactively 'split-window-horizontally)
  (set-window-buffer (next-window) (other-buffer))
  (other-window 1))

(bind-key "C-x 2" 'rl/split-window-vertically)
(bind-key "C-x 3" 'rl/split-window-horizontally)

;; ------------------------------ better page down/up
;; http://snarfed.org/emacs_page_up_page_down
(defun rl/page-down ()
  "Move cursor one page down"
  (interactive)
  (condition-case nil (scroll-up)
    (end-of-buffer (goto-char (point-max)))))

(defun rl/page-up ()
  "Move cursor one page up"
  (interactive)
  (condition-case nil (scroll-down)
    (beginning-of-buffer (goto-char (point-min)))))

(bind-key "C-v" 'rl/page-down)
(bind-key "M-v" 'rl/page-up)

;; ------------------------------ switch window
(defun rl/previous-window ()
  "Select previous window"
  (interactive)
  (other-window -1))

(bind-key "M-o" 'other-window)
(bind-key "M-O" 'rl/previous-window)

(use-package windmove
  :config (windmove-default-keybindings 'shift))

;; ------------------------------ winner
(use-package winner
  :init (winner-mode 1))


(provide 'setup-window)
