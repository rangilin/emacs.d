(defvar rl--ibuffer-filter-groups
  `(("default"
     ("Dired" (mode . dired-mode))
     ("Emacs" (or (name . "\*Messages\*")
                  (name . "\*Warnings\*")
                  (name . "\*Completions\*")
                  (name . "\*Compile-Log\*")
                  (name . "\*Backtrace\*")))
     ("Help" (or (mode . man-mode)
                 (mode . woman-mode)
                 (mode . info-mode)
                 (mode . help-mode)))
     ("Org" (mode . org-mode))
     ("SQL client" (mode . sql-interactive-mode))
     ("Terminal" (or (mode . term-mode)
                     (mode . shell-mode)
                     (mode . eshell-mode)))
     ("Temporary" (name . "\*.*\*")))))


(defvar rl--ibuffer-format
  '((mark modified read-only
          " " (name 24 24 :left :elide)
          " " (readable-size 9 -1 :right)
          " " (mode 16 16 :left :elide)
          " " filename-and-process)))


(defun rl-init-module-editor ()
  "Initialize editor module."
  (rl--load-editor-packages)
  (rl--set-up-navigation)
  (rl--set-up-marks)
  (rl--set-up-pairs)
  (rl--set-up-revert-buffer)
  (rl--set-up-undo)
  (rl--set-up-abbrev)
  (rl--set-up-indentation)
  (rl--set-up-tabs))


(defun rl-mark-line (&optional arg)
  "Mark a line."
  (interactive "p")
  (beginning-of-line)
  (let ((here (point)))
    (dotimes (i arg)
      (end-of-line))
    (set-mark (point))
    (goto-char here)))


(defun rl-mark-sentence (&optional arg)
  "Mark a sentence."
  (interactive "P")
  (backward-sentence)
  (mark-end-of-sentence arg))


(defun rl-refresh-buffer ()
  "Referesh current buffer."
  (interactive)
  (revert-buffer nil t nil)
  (message "buffer is refreshed"))


(defun rl-ibuffer-mode-hook ()
  (ibuffer-switch-to-saved-filter-groups "default"))


;; http://www.emacswiki.org/emacs/BackToIndentationOrBeginning
(defun rl-back-to-indentation-or-beginning ()
  "Back to indentation or beginning of current line"
  (interactive "^")
  (if (bound-and-true-p visual-line-mode)
      (beginning-of-visual-line)
      (when (= (point) (progn (back-to-indentation) (point)))
        (beginning-of-line))))


;; http://stackoverflow.com/a/1249665/554279
(defun rl-horizontal-recenter ()
  "make the point horizontally centered in the window"
  (interactive)
  (let ((mid (/ (window-width) 2))
        (line-len (save-excursion (end-of-line) (current-column)))
        (cur (current-column)))
    (if (< mid cur) (set-window-hscroll (selected-window) (- cur mid)))))


(defun rl-insert-newline-above ()
  "Insert a newline above the current line."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))


(defun rl-insert-newline-below ()
  "Insert a newline below the current line."
  (interactive)
  (end-of-line)
  (newline)
  (indent-according-to-mode))


(defun rl--load-editor-packages ()
  "Load editor packages."

  ;; Make emacs trim whitespace smartly.
  (use-package ws-butler
    :ensure t
    :diminish ws-butler-mode
    :config
    (ws-butler-global-mode 1))

  ;; use ibuffer
  (use-package ibuffer
    :bind ("C-x C-b" . ibuffer)
    :config
    (setq-default ibuffer-show-empty-filter-groups nil)
    (setq-default ibuffer-saved-filter-groups rl--ibuffer-filter-groups)
    (setq-default ibuffer-formats rl--ibuffer-format)

    ;; make size column show human-readable description
    (define-ibuffer-column readable-size
      (:name "Size" :inline t)
      (cond
       ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fK" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))

  (add-hook 'ibuffer-mode-hook 'rl-ibuffer-mode-hook)))


(defun rl--set-up-tabs ()
  "Set up tab behaviors."
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq-default tab-always-indent 'complete))


(defun rl--set-up-navigation ()
  "Set up navigation."

  (use-package ace-window
    :ensure t
    :init
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
  (use-package avy
    :ensure t
    :init
    (setq avy-background t))

  ;; move cursor to top or bottom when it can not scroll
  (setq-default scroll-error-top-bottom t)

  (bind-key "C-S-l" 'rl-horizontal-recenter)
  (bind-key "C-a" 'rl-back-to-indentation-or-beginning))


(defun rl--set-up-pairs ()
  (show-paren-mode 1)
  (electric-pair-mode 1))


(defun rl--set-up-marks ()
  "Set up marks."

  ;; Make marked characters will be replaced when changed.
  (delete-selection-mode 1)
  ;; Set up some marking keybindings.
  (bind-key "M-L" 'rl-mark-line)
  (bind-key "M-S" 'rl-mark-sentence)
  (bind-key "M-X" 'mark-sexp)
  (bind-key "M-D" 'mark-defun))


(defun rl--set-up-revert-buffer ()
  "Set up buffer revert behaviors."
  (use-package autorevert
    :diminish auto-revert-mode
    :init
    (setq auto-revert-verbose nil)
    (setq global-auto-revert-non-file-buffers t)
    :config
    (global-auto-revert-mode 1)
    (bind-key "<f5>" 'rl-refresh-buffer)))


(defun rl--set-up-undo ()
  (use-package undo-tree
    :ensure t
    :diminish undo-tree-mode
    :config
    (global-undo-tree-mode)))


(defun rl--set-up-abbrev ()
  (use-package abbrev
    :diminish abbrev-mode
    :config
    (if (file-exists-p abbrev-file-name)
        (quietly-read-abbrebv-file))))


(defun rl--set-up-indentation ()
  (bind-key "<M-S-return>" 'rl-insert-newline-above)
  (bind-key "<S-return>" 'rl-insert-newline-below))



(provide 'module-editor)
