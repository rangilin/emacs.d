;;----------------------------------------------------------------------------
;; Make "C-x o" prompt for target window
;;----------------------------------------------------------------------------
(require-package 'switch-window)
(require 'switch-window)

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

;;----------------------------------------------------------------------------
;; Rearrange split windows
;;----------------------------------------------------------------------------
(defun split-window-horizontally-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-horizontally))))

(defun split-window-vertically-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-vertically))))


(provide 'init-window)
