;;
;; All operation related to cursor, such as moving cursor, mark.
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

;; ------------------------------ multiple cursors
(use-package multiple-cursors
  :init
  (progn
    (setq-default mc/list-file (expand-file-name ".mc-lists.el" rangi/gen-dir)))
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-S-c C->" . mc/mark-more-like-this-extended)))

;; ------------------------------ sub word
(global-subword-mode)

;; solve issue that subword not support shift selection at my version
(defadvice subword-forward (before advice-subword-forward activate)
  (handle-shift-selection))
(defadvice subword-backward (before advice-subword-backward activate)
  (handle-shift-selection))


;; ------------------------------ mark

;; so I can pop mark multiple time with C-u C-@ C-@...
(setq-default set-mark-command-repeat-pop t)

;; ------------------------------ ace jump
(use-package ace-jump-mode
  :bind (("C-;" . ace-jump-mode)))

(provide 'setup-cursor)
