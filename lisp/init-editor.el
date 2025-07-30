;;; init-editor.el --- Editor configurations -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous editor settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :config

  ;; enable subword
  (global-subword-mode)
  (diminish 'subword-mode)

  ;; delete selection
  (delete-selection-mode)

  ;; add newline at EOF
  (setq require-final-newline t)
  ;; delete trailing whitespace on save
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  ;; hide trailing whiespace in minibuffer when inactive
  (add-hook 'minibuffer-inactive-mode-hook (lambda () (setq show-trailing-whitespace nil)))

  ;; auto pair
  (electric-pair-mode 1)

  ;; zap
  (bind-key "M-z" 'zap-up-to-char)
  (bind-key "M-Z" 'zap-to-char)

  ;; change case smartly
  (bind-key "M-u" 'upcase-dwim)
  (bind-key "M-l" 'downcase-dwim)
  (bind-key "M-c" 'capitalize-dwim)

  ;; backward delete
  (define-key key-translation-map (kbd "C-h") (kbd "DEL"))
  (define-key key-translation-map (kbd "C-S-h") (kbd "C-S-<backspace>"))
  (bind-key "C-M-h" 'backward-kill-word)

  ;; fonts
  (let ((f "M PLUS 1 Code 16"))
    (set-frame-font f nil)
    (add-to-list 'default-frame-alist `(font . ,f))))


;; move text
(use-package move-text
  :load-path "site-lisp/move-text"
  :config
  (move-text-default-bindings))


;; transfer lines of string to array like data
;; modified based on https://news.ycombinator.com/item?id=22131815
(defun rangi-arrayify (start end quote)
  "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
  (interactive "r\nMQuote: ")
  (let ((insertion
         (mapconcat
          (lambda (x) (format "%s%s%s" quote x quote))
          (split-string (buffer-substring start end) "\n" t "\s") ", ")))
    (delete-region start end)
    (insert insertion)))

(global-set-key (kbd "C-c e a") 'rangi-arrayify)


;;;;;;;;;;;;
;; Buffer ;;
;;;;;;;;;;;;

;; create new buffer
(defun rangi-new-buffer ()
  "create a new buffer with specified name"
  (interactive)
  (let ((buffer (generate-new-buffer (read-string "Enter buffer name: " "*scratch*"))))
    (set-buffer-major-mode buffer)
    (switch-to-buffer buffer)))

(bind-key "C-c b n" 'rangi-new-buffer)


;; set up ibuffer
(use-package ibuffer
  :bind (("C-x C-b" . ibuffer))
  :config
  ;; don't show empty group
  (setq-default ibuffer-show-empty-filter-groups nil)

  ;; define a ibuffer column that show human readable size of the buffer
  (define-ibuffer-column readable-size
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fK" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))

  ;; define ibuffer columns
  (setq-default ibuffer-formats
		'((mark modified read-only
			" " (name 60 60 :left :elide)
			" " (readable-size 9 -1 :right)
			" " (mode 16 16 :left :elide)
			" " filename-and-process)))

  ;; unbind print so I don't accidentally print all of my buffers
  (unbind-key "P" ibuffer-mode-map))


;; auto refresh buffer
(use-package autorevert
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode 1)

  ;; don't print anything after refresh
  (setq auto-revert-verbose nil))


;; refresh buffer
(defun rangi-refresh-buffer ()
  "Referesh current buffer."
  (interactive)
  (revert-buffer nil t nil)
  (message "buffer is refreshed"))

(bind-key "C-c b r" 'rangi-refresh-buffer)



;;;;;;;;;;;;;;;
;; Auto Save ;;
;;;;;;;;;;;;;;;

;; prevent `auto-save-list' empty dir created
(setq auto-save-list-file-prefix nil)

;; set up autosave directory
(setq rangi-auto-save-directory (expand-file-name "auto-save" rangi-emacs-cache-directory))
(unless (file-exists-p rangi-auto-save-directory)
  (make-directory rangi-auto-save-directory))

;; put auto save files in to autosave dir
(setq auto-save-file-name-transforms `((".*" ,rangi-auto-save-directory t)))



;;;;;;;;;;;;
;; Backup ;;
;;;;;;;;;;;;

;; set up backup directory
(setq rangi-backup-directory (expand-file-name "backup" rangi-emacs-cache-directory))
(unless (file-exists-p rangi-backup-directory)
  (make-directory rangi-backup-directory))

;; put backup files into backup directory
(setq backup-directory-alist `((".*" . ,rangi-backup-directory)))

;; use copy to backup files
(setq backup-by-copying t)
;; number version backup files
(setq version-control t)
;; kept no old backups
(setq kept-old-versions 0)
;; kept at most this many backups
(setq kept-new-versions 5)
;; delete old version of backup automatically
(setq delete-old-versions t)



;;;;;;;;;;;;;;;;
;; Navigation ;;
;;;;;;;;;;;;;;;;

;; keys for navigation around
(defvar rangi-navigation-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'avy-goto-char-timer)
    (define-key map "i" 'imenu)
    (define-key map "l" 'goto-line)
    (define-key map "L" 'avy-goto-line)
    map)
  "Keymap for navigation in the editor.")

(bind-key "M-g" rangi-navigation-map)

;; move where I meant
(use-package mwim
  :load-path "site-lisp/mwim"
  :bind (("C-a" . mwim-beginning-of-code-or-line)
	 ("C-e" . mwim-end-of-code-or-line)))

;; move cursor to top or bottom of the buffer when it cannot be scrolled anymore
(setq scroll-error-top-bottom t)

;; recenter in the center of a horizontal line
;; http://stackoverflow.com/a/1249665/554279
(defun rangi-horizontal-recenter ()
  "make the point horizontally centered in the window"
  (interactive)
  (let ((mid (/ (window-width) 2))
        (line-len (save-excursion (end-of-line) (current-column)))
        (cur (current-column)))
    (if (< mid cur) (set-window-hscroll (selected-window) (- cur mid)))))
(bind-key "C-S-l" 'rangi-horizontal-recenter)


;; jump between characters/words
(use-package avy
  :ensure t
  :pin gnu
  :config
  (setq avy-background t))


;;;;;;;;;;;;;;;;;
;; Indentation ;;
;;;;;;;;;;;;;;;;;

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

(bind-key "<C-S-return>" 'rangi-insert-newline-above)
(bind-key "<S-return>" 'rangi-insert-newline-below)

;; indent with space by default
(setq-default indent-tabs-mode nil)
;; indent 2 spaces by default
(setq-default tab-width 2)
;; indent automatically
(electric-indent-mode 1)



;;;;;;;;;;;;;;;
;; Selection ;;
;;;;;;;;;;;;;;;

;; expand region
(use-package expand-region
  :ensure t
  :pin gnu
  :bind
  (("C-=" . er/expand-region)))


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



;;;;;;;;;;;;
;; Cursor ;;
;;;;;;;;;;;;


(use-package multiple-cursors
  :ensure t
  :pin nongnu
  :init
  ;; save mc file in cache directory
  (setq-default mc/list-file (expand-file-name "mc-lists.el" rangi-emacs-cache-directory))
  :bind-keymap (("C-c e c" . rangi-mc-repeat-map))
  :bind
  (("s-<mouse-1>" . mc/add-cursor-on-click)
   ("C-c e l" . mc/edit-lines)
   (:repeat-map rangi-mc-repeat-map
                ("a" . mc/mark-all-dwim)
                ("." . mc/mark-next-like-this)
                ("," . mc/mark-previous-like-this)
                (">" . mc/unmark-next-like-this)
                ("<" . mc/unmark-previous-like-this))))


;;;;;;;;;;;;;;;;;
;; Undo / Redo ;;
;;;;;;;;;;;;;;;;;

(use-package undo-tree
  :diminish
  :ensure t
  :pin gnu
  :config
  ;; show time differences in visualizer
  (setq undo-tree-visualizer-timestamps t)
  ;; show diff between changes in visualizer
  (setq undo-tree-visualizer-diff t)

  ;; store undo histories
  (setq undo-tree-auto-save-history t)
  (let ((dir (expand-file-name "undo" rangi-emacs-cache-directory)))
    (unless (file-exists-p dir) (make-directory dir))
    (setq undo-tree-history-directory-alist `(("." . ,dir))))

  ;; compress history files
  (defadvice undo-tree-make-history-save-file-name
      (after undo-tree activate)
    (setq ad-return-value (concat ad-return-value ".gz")))

  (global-undo-tree-mode))




(provide 'init-editor)


;; ;;
;; ;; Spelling
;; ;; ----------------------------------------------------------------------------
;; ;;

;; ;; use aspell as ispell's program
;; (use-package ispell
;;   :config
;;   (setq ispell-program-name "aspell")
;;   (setq ispell-personal-dictionary "~/Documents/aspell-dictionaries/aspell.en_US.pws")
;;   (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))


;; ;; on-the-fly spell checking
;; (use-package flyspell
;;   :delight flyspell-mode
;;   :init
;;   (setq-default flyspell-issue-message-flag nil)
;;   (setq-default flyspell-issue-welcome-flag nil)

;;   ;; enable flyspell-prog-mode in all prog-modes
;;   (add-hook 'prog-mode-hook (lambda () (flyspell-prog-mode)))
;;   ;; enable flyspell in text mode
;;   (add-hook 'text-mode-hook (lambda () (turn-on-flyspell))))


;; ;;
;; ;; sudo edit
;; ;; ----------------------------------------------------------------------------
;; ;;

;; (use-package sudo-edit
;;   :ensure t
;;   :bind (:map ctl-x-map
;;               ("M-s" . sudo-edit)))


;; (provide 'init-editor)


;; ;;
;; ;; better keyboard quit
;; ;; ----------------------------------------------------------------------------
;; ;;

;; ;; https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/#h:1e468b2a-9bee-4571-8454-e3f5462d9321

;; (defun rangi-keyboard-quit-dwim ()
;;   (interactive)
;;   (cond ((region-active-p)
;;          (keyboard-quit))
;;         ((derived-mode-p 'completion-list-mode)
;;          (delete-completion-window))
;;         ((> (minibuffer-depth) 0)
;;          (abort-recursive-edit))
;;         (t (keyboard-quit))))

;; (bind-key "C-g" 'rangi-keyboard-quit-dwim)
