(require 'use-package)

;; ------------------------------ split window
(defun rangi/split-window-vertically ()
  "Split window vertically. Focus to the new window and display a recent used
buffer in it"
  (interactive)
  (call-interactively 'split-window-vertically)
  (set-window-buffer (next-window) (other-buffer))
  (other-window 1))

(defun rangi/split-window-horizontally ()
  "Split window horizontally. Focus to the new window and display a recent used
buffer in it"
  (interactive)
  (call-interactively 'split-window-horizontally)
  (set-window-buffer (next-window) (other-buffer))
  (other-window 1))

(bind-key "C-x 2" 'rangi/split-window-vertically)
(bind-key "C-x 3" 'rangi/split-window-horizontally)

;; ------------------------------ better page down/up
;; http://snarfed.org/emacs_page_up_page_down
(defun rangi/page-down ()
  "Move cursor one page down"
  (interactive)
  (condition-case nil (scroll-up)
    (end-of-buffer (goto-char (point-max)))))

(defun rangi/page-up ()
  "Move cursor one page up"
  (interactive)
  (condition-case nil (scroll-down)
    (beginning-of-buffer (goto-char (point-min)))))

(bind-key "C-v" 'rangi/page-down)
(bind-key "M-v" 'rangi/page-up)

;; ------------------------------ switch window
(defun rangi/previous-window ()
  "Select previous window"
  (interactive)
  (other-window -1))

(bind-key "M-o" 'other-window)
(bind-key "M-O" 'rangi/previous-window)

(use-package windmove
  :config (windmove-default-keybindings 'shift))

;; ------------------------------ winner
(use-package winner
  :init (winner-mode 1))

;; ------------------------------ resizing
(defun rangi/resize-window ()
  (interactive)
  (set-temporary-overlay-map
    (let ((map (make-sparse-keymap)))
        (define-key map (kbd "<left>") 'shrink-window-horizontally)
        (define-key map (kbd "<right>") 'enlarge-window-horizontally)
        (define-key map (kbd "<down>") 'shrink-window)
        (define-key map (kbd "<up>") 'enlarge-window)
        map) t))

(bind-key "C-c w" 'rangi/resize-window);

(provide 'setup-window)
