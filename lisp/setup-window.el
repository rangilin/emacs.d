(require 'use-package)

;; split window vertically if window width is more than 80, otherwise do it horizontally
(setq split-height-threshold nil)
(setq split-width-threshold 120)

;; ------------------------------ split window
(defun rangi/split-window-vertically ()
  "Split window vertically. Focus to the new window and display a recent used
buffer in it"
  (interactive)
  (call-interactively 'split-window-vertically)
  ;(set-window-buffer (next-window) (other-buffer))
  (other-window 1))

(defun rangi/split-window-horizontally ()
  "Split window horizontally. Focus to the new window and display a recent used
buffer in it"
  (interactive)
  (call-interactively 'split-window-horizontally)
  ;(set-window-buffer (next-window) (other-buffer))
  (other-window 1))

(bind-key "C-x 2" 'rangi/split-window-vertically)
(bind-key "C-x 3" 'rangi/split-window-horizontally)

;; ------------------------------ better page down/up
;; http://snarfed.org/emacs_page_up_page_down
(defun rangi/page-down ()
  "Move cursor one page down"
  (interactive "^")
  (condition-case nil (scroll-up)
    (end-of-buffer (goto-char (point-max)))))

(defun rangi/page-up ()
  "Move cursor one page up"
  (interactive "^")
  (condition-case nil (scroll-down)
    (beginning-of-buffer (goto-char (point-min)))))

(bind-key "C-v" 'rangi/page-down)
(bind-key "M-v" 'rangi/page-up)

;; ------------------------------ ace window
(use-package ace-window
  :bind (("C-x o" . ace-window)
         ("C-x C-o" . ace-window))
  :config
  (progn
    (set-face-attribute
     'aw-leading-char-face nil
     :inherit 'ace-jump-face-foreground :height 3.0)
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))

;; ------------------------------ winner
(use-package winner
  :init (winner-mode 1))

;; ------------------------------ resizing
(defun rangi/resize-window ()
  (interactive)
  (set-transient-map
    (let ((map (make-sparse-keymap)))
        (define-key map (kbd "<left>") 'shrink-window-horizontally)
        (define-key map (kbd "<right>") 'enlarge-window-horizontally)
        (define-key map (kbd "<down>") 'shrink-window)
        (define-key map (kbd "<up>") 'enlarge-window)
        map) t))

(bind-key "C-c w" 'rangi/resize-window);

(provide 'setup-window)
