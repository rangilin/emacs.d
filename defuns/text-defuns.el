;; Text manipulation related functions

;; -------------------------------------------------- forward/backward block
;; ergoemacs-keybindings/ergoemacs-functions.el

(defun next-block (&optional number)
  "Move cursor forward to the beginning of next text block.
A text block is separated by 2 empty lines (or line with just whitespace).
In most major modes, this is similar to `forward-paragraph', but this command's behavior is the same regardless of syntax table.

With a prefix argument NUMBER, move forward NUMBER blocks.
With a negative prefix argument NUMBER, move backward NUMBER blocks."
  (interactive "p")
  (if (and number
           (> 0 number))
      (previous-block (- 0 number))
  (if (search-forward-regexp "\n[[:blank:]\n]*\n+" nil "NOERROR" number)
      (progn (backward-char))
    (progn (goto-char (point-max))))))

(defun previous-block (&optional number)
  "Move cursor backward to previous text block."
  (interactive "p")
  (if (and number
           (> 0 number))
      (next-block (- 0 number))
    (if (search-backward-regexp "\n[\t\n ]*\n+" nil "NOERROR" number)
        (progn
          (skip-chars-backward "\n\t ")
          (forward-char 1))
      (progn (goto-char (point-min))))))

;; -------------------------------------------------- Toggle letter case
;; http://ergoemacs.org/emacs/modernization_upcase-word.html
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

;; -------------------------------------------------- indentations
(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

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


(defun back-to-indentation-or-beginning-of-line ()
  "Move point to the first non-whitespace character, move to beginning of the line if repeated"
  (interactive)
  (if (eq last-command this-command)
      (call-interactively 'move-beginning-of-line)
    (back-to-indentation)))

;; -------------------------------------------------- scrolling
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

;; -------------------------------------------------- comments
;; http://stackoverflow.com/questions/9688748/emacs-comment-uncomment-current-line
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

