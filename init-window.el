;; -------------------------------------------------- winner mode
(winner-mode 1)

;;----------------------------------------------------------------------------
;; By calling split-window-vertically or split-window-horizontal with this
;; function. It will focus and display other buffer in the new window.
;;----------------------------------------------------------------------------
(defun focus-and-show-other-buffer-after (split-function)
  (lexical-let ((s-f split-function))
    (lambda ()
      (interactive)
      (funcall s-f)
      (set-window-buffer (next-window) (other-buffer))
      (other-window 1))))

;; FIXME : too wet :( gonna learn some lisp
(defun show-other-buffer-after (split-function)
  (lexical-let ((s-f split-function))
    (lambda ()
      (interactive)
      (funcall s-f)
      (set-window-buffer (next-window) (other-buffer)))))

;;----------------------------------------------------------------------------
;; Rearrange split windows
;;----------------------------------------------------------------------------
(defun split-window-vertically-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (show-other-buffer-after 'split-window-horizontally))))

(defun split-window-horizontally-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (show-other-buffer-after 'split-window-vertically))))

(provide 'init-window)
