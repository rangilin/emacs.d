;;; module-editor.el

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


(defun rl/initialize-module-editor ()
  "Initialize editor module."
  (rl--load-editor-packages)
  (rl--set-up-navigation)
  (rl--set-up-marks)
  (rl--set-up-pairs)
  (rl--set-up-tabs))


(defun rl/mark-line (&optional arg)
  "Mark a line."
  (interactive "p")
  (beginning-of-line)
  (let ((here (point)))
    (dotimes (i arg)
      (end-of-line))
    (set-mark (point))
    (goto-char here)))


(defun rl/mark-sentence (&optional arg)
  "Mark a sentence."
  (interactive "P")
  (backward-sentence)
  (mark-end-of-sentence arg))


(defun rl/refresh-buffer ()
  "Referesh current buffer."
  (interactive)
  (revert-buffer nil t nil)
  (message "buffer is refreshed"))


(defun rl/ibuffer-mode-hook ()
  (ibuffer-switch-to-saved-filter-groups "default"))


(defun rl--load-editor-packages ()
  "Load editor packages."

  ;; Make emacs trim whitespace smartly.
  (use-package ws-butler
    :ensure t
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

  (add-hook 'ibuffer-mode-hook 'rl/ibuffer-mode-hook)))


(defun rl--set-up-tabs ()
  "Set up tab behaviors."
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq-default tab-always-indent 'complete))


(defun rl--set-up-navigation ()
  "Set up keybindings for navigation."
  (bind-key "M-g c" #'goto-char)
  (bind-key "M-g l" #'goto-line))


(defun rl--set-up-pairs ()
  (show-paren-mode 1)
  (electric-pair-mode 1))


(defun rl--set-up-marks ()
  "Set up marks."

  ;; Make marked characters will be replaced when changed.
  (delete-selection-mode 1)
  ;; Set up some marking keybindings.
  (bind-key "M-L" #'rl/mark-line)
  (bind-key "M-S" #'rl/mark-sentence)
  (bind-key "M-X" #'mark-sexp)
  (bind-key "M-D" #'mark-defun))


(defun rl--set-up-revert-buffer ()
  "Set up buffer revert behaviors."
  (global-auto-revert-mode 1)
  (setq-default global-auto-revert-non-file-buffers t)
  (setq-default auto-revert-verbose nil)
  (bind-key "<f5>" 'rl/refresh-buffer))



(provide 'module-editor)
