;; -------------------------------------------------- smarter move-text
(defun rangi-move-text-up (arg)
  (interactive "*p")
  (move-text-up arg)
  (if (point-is-at-upper-window)
      (recenter-top-bottom (truncate (/ (window-text-height) 2)))))

(defun rangi-move-text-down (arg)
  (interactive "*p")
  (move-text-down arg)
  (if (region-active-p)
      (exchange-point-and-mark))
  (if (point-is-at-lower-window)
      (recenter-top-bottom (truncate (/ (window-text-height) 2)))))

(defun point-related-to-window ()
  (cdr (nth 6 (posn-at-point))))

(defun point-is-at-upper-window ()
  (<= (point-related-to-window) (truncate (/ (window-text-height) 2))))

(defun point-is-at-lower-window ()
  (> (point-related-to-window) (truncate (/ (window-text-height) 2))))

;; -------------------------------------------------- split function
(defun focus-and-show-other-buffer-after (split-function)
  (lexical-let ((s-f split-function))
    (lambda ()
      (interactive)
      (funcall s-f)
      (set-window-buffer (next-window) (other-buffer))
      (other-window 1))))
(defun show-other-buffer-after (split-function)
  (lexical-let ((s-f split-function))
    (lambda ()
      (interactive)
      (funcall s-f)
      (set-window-buffer (next-window) (other-buffer)))))

;; -------------------------------------------------- rearrange split windows
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

;; -------------------------------------------------- better scrolling
(defun scroll-cursor-down ()
  "Move cursor one page down"
  (interactive)
  (condition-case nil (scroll-up)
    (end-of-buffer (goto-char (point-max)))))

(defun scroll-cursor-up ()
  "Move cursor one page up"
  (interactive)
  (condition-case nil (scroll-down)
    (beginning-of-buffer (goto-char (point-min)))))

;; -------------------------------------------------- indentation
(defun insert-newline-above ()
  "Insert a newline above the current line."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun insert-newline-below ()
  "Insert a newline below the current line."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; -------------------------------------------------- comments
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position))
            (deactivate-mark))
        (comment-or-uncomment-region beg end)
        (next-logical-line)))


(provide 'defun)
