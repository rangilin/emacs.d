(setq-default
 show-trailing-whitespace t
 indent-tabs-mode nil
 tab-width 4
 line-number-mode 1
 column-number-mode 1
 x-select-enable-clipboard t)

;;----------------------------------------------------------------------------
;; Turn off whitespace visualization in some modes
;;----------------------------------------------------------------------------
(dolist (hook '(eshell-mode-hook shell-mode-hook))
  (add-hook hook
            (lambda () (setq show-trailing-whitespace nil))))

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
;; Show matching parenthesis
;; ----------------------------------------------------------------------------
(show-paren-mode 1)

;;----------------------------------------------------------------------------
;; duplicate-thing
;; ----------------------------------------------------------------------------
(require-package 'duplicate-thing)
(require 'duplicate-thing)


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

;;----------------------------------------------------------------------------
;; Scroll behaviors
;;----------------------------------------------------------------------------

;; mouse scrolling three line at a time
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1)))

;; don't accelerate mouse scrolling
(setq mouse-wheel-progressive-speed nil)

;; scroll window under mouse
(setq mouse-wheel-follow-mouse 't)

;; make emacs scroll one line instead of half screen when cursor meet top/bottom of the screen
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;;----------------------------------------------------------------------------
;; Undo-tree
;;----------------------------------------------------------------------------
(require-package 'undo-tree)
(require 'undo-tree)
(eval-after-load "undo-tree"
  (define-key undo-tree-map (kbd "C-/") nil))

(global-undo-tree-mode 1)

;;----------------------------------------------------------------------------
;; Move-text
;;----------------------------------------------------------------------------
(require-package 'move-text)
(require 'move-text)

;;----------------------------------------------------------------------------
;; expand-region
;;----------------------------------------------------------------------------
(require-package 'expand-region)
(require 'expand-region)

;;----------------------------------------------------------------------------
;; forward/backward block
;; https://code.google.com/p/ergoemacs/source/browse/ergoemacs/ergoemacs-keybindings/ergoemacs-functions.el
;;----------------------------------------------------------------------------

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

;;----------------------------------------------------------------------------

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


(provide 'init-editing)
