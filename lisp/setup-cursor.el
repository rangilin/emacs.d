;;
;; All operation related to cursor, such as moving cursor.
;;
(require 'use-package)
(require 'variables)
(require 'thingatpt)

;; ------------------------------ horizontal recenter
;; http://stackoverflow.com/a/1249665/554279
(defun rangi/horizontal-recenter ()
  "make the point horizontally centered in the window"
  (interactive)
  (let ((mid (/ (window-width) 2))
        (line-len (save-excursion (end-of-line) (current-column)))
        (cur (current-column)))
    (if (< mid cur) (set-window-hscroll (selected-window) (- cur mid)))))

(bind-key "C-S-l" 'rangi/horizontal-recenter)

;; ------------------------------ Multiple cursors
(use-package multiple-cursors
  :init
  (progn
    (setq-default mc/list-file (expand-file-name ".mc-lists.el" rangi/gen-dir)))
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-S-c C->" . mc/mark-more-like-this-extended)))

;; ------------------------------ Sub word
(global-subword-mode)

;; solve issue that subword not support shift selection at my version
(defadvice subword-backward (before handle-shift-selection activate)
  (handle-shift-selection))
(defadvice subword-forward (before handle-shift-selection activate)
  (handle-shift-selection))

;; ------------------------------ Mark

;; so I can pop mark multiple time with C-u C-@ C-@...
(setq-default set-mark-command-repeat-pop t)

;; ------------------------------ Ace Jump
(use-package ace-jump-mode
  :bind (("C-;" . ace-jump-mode)))

(provide 'setup-cursor)
