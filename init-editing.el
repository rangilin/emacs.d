(setq-default
 show-trailing-whitespace t
 indent-tabs-mode nil
 tab-width 4
 line-number-mode 1
 column-number-mode 1
 x-select-enable-clipboard t
 line-spacing 0.1)


;;----------------------------------------------------------------------------
;; delete/replace active region when typing
;;----------------------------------------------------------------------------
(delete-selection-mode 1)

;;----------------------------------------------------------------------------
;; Auto refresh buffers
;;----------------------------------------------------------------------------
(global-auto-revert-mode 1)
;; also refresh non-file buffer like dired
(setq global-auto-revert-non-file-buffers t)
;; do it sliently
(setq auto-revert-verbose nil)

;;----------------------------------------------------------------------------
;; Toggle letter case of current word or active region
;; http://ergoemacs.org/emacs/modernization_upcase-word.html
;;----------------------------------------------------------------------------
(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word) ) )
        (setq p1 (car bds) p2 (cdr bds)) ) )

    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps") )
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps") )
         ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all caps") )
         (t (put this-command 'state "all lower") ) ) ) )

    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "all lower")) )
    ) )

;;----------------------------------------------------------------------------
;; Scroll by moving cursor instead of screen
;;----------------------------------------------------------------------------
(defun scroll-point-down ()
  "Move cursor one page down"
  (interactive)
  (condition-case nil (scroll-up)
    (end-of-buffer (goto-char (point-max)))))

(defun scroll-point-up ()
  "Move cursor one page up"
  (interactive)
  (condition-case nil (scroll-down)
    (beginning-of-buffer (goto-char (point-min)))))

;;----------------------------------------------------------------------------
;; Scroll behaviors
;;----------------------------------------------------------------------------

;; mouse scrolling one line at a time
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1)))

;; don't accelerate mouse scrolling
(setq mouse-wheel-progressive-speed nil)

;; scroll window under mouse
(setq mouse-wheel-follow-mouse 't)

;; make emacs scroll one line instead of half screen when cursor meet top/bottom of the screen
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)


(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))


(provide 'init-editing)

