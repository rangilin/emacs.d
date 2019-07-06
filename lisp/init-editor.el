;; warn when open file larger than 100MB
(setq large-file-warning-threshold 100000000)

;; highlight matching parentheses
(show-paren-mode 1)

;; pairing parenthesis automatically
(electric-pair-mode 1)

;; show keystrokes right away
(setq echo-keystrokes 0.1)

;; no startup messages
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message nil)

;; no start up screen
(setq inhibit-startup-screen t)

;; hide cursor in inactive windows
(setq cursor-in-non-selected-windows t)

;; make scratch buffer empty
(setq initial-scratch-message nil)

;; open scratch buffer in text mode
(setq initial-major-mode 'text-mode)

;; sentence end after one space line
(setq sentence-end-double-space nil)

;; ask before quit emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; select help window automatically, so it is easier to close it with `q`
(setq help-window-select t)

;; force ask y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; toggle truncate lines
(global-set-key (kbd "C-c e l") 'toggle-truncate-lines)

;; treat Esc like C-g
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; move file to trash when deleted
(setq-default delete-by-moving-to-trash t)

;;
;; adjust settings when open large file to increase performance
;;
;; Here are some common setting that affect Emacs performance
;;
;; 1. hl-line-mode
;; 2. column-number-mode
;; 3. line-number-mode
;; 4. linum-mode
;; 5. non-nil auto-window-vscroll
;;
;; disable them all for a snappy Emacs

(defun rangi-open-large-file-hook ()
  (let ((line-count (count-lines (buffer-end -1) (buffer-end +1)))
        (file-size (or (f-size (buffer-file-name)) 0)))
    (when (or (>= line-count 25000) (>= file-size 5000000))
      (message "Settings are adjusted for large file")
      (font-lock-mode -1)
      (highlight-numbers-mode -1)
      (turn-off-flyspell))
    (when (or (>= line-count 100000) (>= file-size 20000000))
      (message "Change to `fundamental-mode' large file")
      (fundamental-mode))))

(add-hook 'find-file-hook 'rangi-open-large-file-hook)

;; backward delete
(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
(bind-key "C-M-h" 'backward-kill-word)


;;
;; Prettify Symbol
;; ----------------------------------------------------------------------------
;;

(setq prettify-symbols-unprettify-at-point t)
(setq prettify-symbols-alist
      '(("lambda" . 955)))
(global-prettify-symbols-mode t)


;;
;; Whitespace
;; ----------------------------------------------------------------------------
;;

;; show trailing whitespace in prog-mode
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; hide trailing whiespace in minibuffer
(add-hook 'minibuffer-inactive-mode-hook (lambda () (setq show-trailing-whitespace nil)))

;; add newline at EOF
(setq require-final-newline t)

;; delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; delete whitespaces more aggresively
(use-package hungry-delete
  :delight
  :config
  ;; don't delete newline
  (setq hungry-delete-chars-to-skip " \t\f\v")
  (global-hungry-delete-mode))



;;
;; Indentations
;; ----------------------------------------------------------------------------
;;

;; make it easier to insert new line on specific position
(defun rangi-insert-newline-above ()
  "Insert a newline above the current line."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun rangi-insert-newline-below ()
  "Insert a newline below the current line."
  (interactive)
  (end-of-line)
  (newline)
  (indent-according-to-mode))

(global-set-key (kbd "<C-S-return>") 'rangi-insert-newline-above)
(global-set-key (kbd "<S-return>") 'rangi-insert-newline-below)

;; always indent with space
(setq-default indent-tabs-mode nil)

;; indent 2 space by default
(setq-default tab-width 2)

;; try to complete thing-at-point after current line is indented
(setq tab-always-indent 'complete)

;; indent automatically
(electric-indent-mode 1)



;;
;; Selection
;; ----------------------------------------------------------------------------
;;

;; remove selected text when inserting new text
(delete-selection-mode 1)

;; expand-region
(use-package expand-region
  :bind
  (("s-'" . er/expand-region)
   ("s-\"" . er/contract-region)))


;; randomize region
(defun rangi-randomize-region (beg end)
  "Randomize lines in region from BEG to END."
  (interactive "*r")
  (let ((lines (split-string
                (delete-and-extract-region beg end) "\n")))
    (when (string-equal "" (car (last lines 1)))
      (setq lines (butlast lines 1)))
    (apply 'insert
           (mapcar 'cdr
                   (sort (mapcar (lambda (x) (cons (random) (concat x "\n"))) lines)
                         (lambda (a b) (< (car a) (car b))))))))


;;
;; Cursors
;; ----------------------------------------------------------------------------
;;

(use-package multiple-cursors
  :init
  ;; save mc file in autogen directory
  (setq-default mc/list-file (expand-file-name "mc-lists.el" rangi-generated-files-directory))

  :config
  (global-set-key (kbd "s-d") 'mc/mark-next-like-this)
  (global-set-key (kbd "s-D") 'mc/mark-all-dwim)
  (global-set-key (kbd "M-s-d") 'mc/edit-lines))


(use-package beacon
  :delight
  :config
  (beacon-mode 1))


;;
;; Navigation
;; ----------------------------------------------------------------------------
;;

;; enable subword mode
(use-package subword
  :delight
  :config
  (global-subword-mode))

;; move where I mean
(use-package mwim
  :bind
  (("C-a" . mwim-beginning-of-code-or-line)
   ("C-e" . mwim-end-of-code-or-line)))

;; move cursor to top or bottom of the buffer when it cannot be scrolled anymore
(setq-default scroll-error-top-bottom t)

;; recenter in the center of a horizontal line
;; http://stackoverflow.com/a/1249665/554279
(defun rangi-horizontal-recenter ()
  "make the point horizontally centered in the window"
  (interactive)
  (let ((mid (/ (window-width) 2))
        (line-len (save-excursion (end-of-line) (current-column)))
        (cur (current-column)))
    (if (< mid cur) (set-window-hscroll (selected-window) (- cur mid)))))

(global-set-key (kbd "C-S-l") 'rangi-horizontal-recenter)


;; jump between characters/words
(use-package avy
  :config
  (setq avy-background t)

  ;; use this function to active jump
  (defun rangi-active-cursor-jump (arg)
    (interactive "p")
    (message "Jump: (g): character (w): word (l): line (L): line in view")
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "g") 'avy-goto-char-timer)
       (define-key map (kbd "w") 'avy-goto-word-or-subword-1)
       (define-key map (kbd "l") 'goto-line)
       (define-key map (kbd "L") 'avy-goto-line)
       map)
     t))

  (bind-key "s-g" 'avy-goto-char-timer)
  (bind-key "M-g c" 'avy-goto-char-timer)
  (bind-key "s-G" 'rangi-active-cursor-jump))


;; select marks
(defun marker-is-point-p (marker)
  "test if marker is current point"
  (and (eq (marker-buffer marker) (current-buffer))
       (= (marker-position marker) (point))))

(defun push-mark-maybe ()
  "push mark onto `global-mark-ring' if mark head or tail is not current location"
  (if (not global-mark-ring) (error "global-mark-ring empty")
    (unless (or (marker-is-point-p (car global-mark-ring))
                (marker-is-point-p (car (reverse global-mark-ring))))
      (push-mark))))

(defun backward-global-mark ()
  "use `pop-global-mark', pushing current point if not on ring."
  (interactive)
  (push-mark-maybe)
  (when (marker-is-point-p (car global-mark-ring))
    (call-interactively 'pop-global-mark))
  (call-interactively 'pop-global-mark))

(defun forward-global-mark ()
  "hack `pop-global-mark' to go in reverse, pushing current point if not on ring."
  (interactive)
  (push-mark-maybe)
  (setq global-mark-ring (nreverse global-mark-ring))
  (when (marker-is-point-p (car global-mark-ring))
    (call-interactively 'pop-global-mark))
  (call-interactively 'pop-global-mark)
  (setq global-mark-ring (nreverse global-mark-ring)))

(global-set-key (kbd "s-,") 'backward-global-mark)
(global-set-key (kbd "s-.") 'forward-global-mark)


;;
;; Undo & Redo
;; ----------------------------------------------------------------------------
;;

(use-package undo-tree
  :delight
  :bind (("s-z" . undo-tree-undo)
         ("s-Z" . undo-tree-redo))
  :config
  ;; show time differences in visualizer
  (setq undo-tree-visualizer-timestamps t)
  ;; show diff between changes in visualizer
  (setq undo-tree-visualizer-diff t)

  ;; free up key for others
  (unbind-key "C-/" undo-tree-map)

  ;; store undo histories
  (setq undo-tree-auto-save-history t)
  (let ((dir (expand-file-name "undo" rangi-generated-files-directory)))
    (unless (file-exists-p dir) (make-directory dir))
    (setq undo-tree-history-directory-alist `(("." . ,dir))))
  (global-undo-tree-mode))


;;
;; Editing actions
;; ----------------------------------------------------------------------------
;;

;; zap
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;; comment smartly
(global-set-key (kbd "C-/") 'comment-dwim)
;; always comment empty lines
(setq comment-empty-lines t)

;; change case smartly
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

;; move text
(use-package move-text
  :config
  (move-text-default-bindings))

;; regex replace with visual guide
(use-package visual-regexp
  :config
  (bind-key "C-S-s-d" 'vr/mc-mark)
  (bind-key "M-%" 'vr/query-replace)
  (bind-key "s-r" 'vr/query-replace))


(use-package transpose-mark
  :bind (("M-T" . transpose-mark))
  :config
  (defun rangi-transpose-mark-abort-advice (fn &rest args)
    (if (transpose-mark-region-overlay-active)
        (transpose-mark-region-abort)
      (apply fn args)))

  (advice-add 'keyboard-quit :around #'rangi-transpose-mark-abort-advice))


;;
;; Recent files
;; ----------------------------------------------------------------------------
;;

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" rangi-generated-files-directory))
  ;; avoid accidentally access remote file during cleanup
  (setq recentf-auto-cleanup 'never)
  ;; only store 50 items
  (setq recentf-max-menu-items 50)

  ;; save recent files every 5 mins and do it silently
  ;; NOTE: due to how (save-silently t) works, message in minibuffer disappear when recentf save
  (run-at-time nil (* 5 60)
               (lambda ()
                 (let ((save-silently t))
                   (recentf-save-list))))
  (recentf-mode 1))



;;
;; Snippets
;; ----------------------------------------------------------------------------
;;

(use-package yasnippet
  :delight yas-minor-mode
  :config
  (yas-global-mode 1))



;;
;; Spelling
;; ----------------------------------------------------------------------------
;;

;; use aspell as ispell's program
(use-package ispell
  :config
  (setq ispell-program-name "aspell")
  (setq ispell-personal-dictionary "~/Documents/aspell-dictionaries/aspell.en_US.pws")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))


;; on-the-fly spell checking
(use-package flyspell
  :delight (flyspell-mode " Spell")
  :init
  (setq-default flyspell-issue-message-flag nil)
  (setq-default flyspell-issue-welcome-flag nil)

  ;; enable flyspell-prog-mode in all prog-modes
  (add-hook 'prog-mode-hook (lambda () (flyspell-prog-mode)))
  ;; enable flyspell in text mode
  (add-hook 'text-mode-hook (lambda () (turn-on-flyspell))))



;;
;; Help
;; ----------------------------------------------------------------------------
;;

(use-package helpful
  :bind (("<f1> f" . 'helpful-callable)
         ("<f1> F" . 'helpful-function)
         ("<f1> v" . 'helpful-variable)
         ("<f1> k" . 'helpful-key)
         ("<f1> C" . 'helpful-command)
         ("C-c C-d" . 'helpful-at-point)))


(provide 'init-editor)
