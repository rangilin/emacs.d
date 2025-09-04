;;; init-editor.el --- Editor configurations -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous editor settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(add-hook 'minibuffer-inactive-mode-hook
          (lambda () (setq show-trailing-whitespace nil)))

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

;; duplicate
(setq duplicate-line-final-position -1)
(setq duplicate-region-final-position -1)
(bind-key "M-D" 'duplicate-dwim)

;; always render text from left to right, no scan
(setq bidi-paragraph-direction 'left-to-right)
;; handle long line automatically
(global-so-long-mode 1)

;; enable set goal column
(put 'set-goal-column 'disabled nil)


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



;;;;;;;;;;
;; Font ;;
;;;;;;;;;;


;; fonts
(let ((mono-spaced-font "Cascadia Code")
      (proportionately-spaced-font "Noto Sans CJK TC"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 140)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.4)
  (set-face-attribute 'variable-pitch nil
                      :family proportionately-spaced-font :height 1.1))

(use-package ligature
  :load-path "site-lisp/ligature"
  :config
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  (global-ligature-mode t))


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
  :bind (("C-a" . mwim-beginning)
         ("C-e" . mwim-end)))

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


;;;;;;;;;;
;; Copy ;;
;;;;;;;;;;


;; make mouse yank at cursor instead of at where it click
(setq mouse-yank-at-point t)
;; preserve existing clipboard text into kill ring before replace it
(setq save-interprogram-paste-before-kill t)


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

  (global-undo-tree-mode))



;;;;;;;;;;;;;;
;; Spelling ;;
;;;;;;;;;;;;;;


(use-package ispell
  :config
  (setq ispell-program-name "aspell")
  (setq ispell-personal-dictionary "~/Documents/misc/aspell-dictionaries/aspell.en_US.pws")
  (setq ispell-extra-args '("--run-together" "--camel-case" "--lang=en_US")))


(use-package flyspell
  :diminish flyspell-mode
  :config
  ;; only check edited text
  (setq flyspell-check-changes t)
  ;; no show messages
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)

  ;; enable flyspell-prog-mode in all prog-modes test
  (add-hook 'prog-mode-hook (lambda () (flyspell-prog-mode)))
  ;; enable flyspell in text mode
  (add-hook 'text-mode-hook (lambda () (turn-on-flyspell))))




(provide 'init-editor)
