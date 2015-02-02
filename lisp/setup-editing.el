(require 'use-package)
(require 'functions)

(setq-default tab-width 2)
(setq-default comment-empty-lines t)
(setq-default indent-tabs-mode nil)
(setq-default x-select-enable-clipboard t)
(setq-default require-final-newline t)

(delete-selection-mode 1)
(show-paren-mode 1)


;; ------------------------------ newline & Indent
(bind-key "RET" 'newline-and-indent)

;; ------------------------------ join line
(defun rangi/join-below-line ()
  (interactive)
  (join-line 1))

(bind-key "C-j" 'rangi/join-below-line)
(bind-key "C-S-j" 'join-line)

;; ------------------------------ browse kill ring
(use-package browse-kill-ring
  :bind (("C-S-y" . browse-kill-ring)))

;; ------------------------------ expand region
(use-package expand-region
  :bind (("C-'" . er/expand-region)
         ("C-\"" . er/contract-region))
  :init
  (progn
    (defun rangi/er-org-mode-hook ()
      (bind-key "C-'" 'er/expand-region org-mode-map))
    (add-hook 'org-mode-hook 'rangi/er-org-mode-hook)))

;; ------------------------------ change inner
(use-package change-inner
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

;; ------------------------------ undo
(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (progn
    (bind-key "C-/" nil undo-tree-map)
    (bind-key "C-?" nil undo-tree-map)
    (global-undo-tree-mode 1)))

;; ------------------------------ yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (progn
    (let ((snippets-dir (f-expand "snippets" user-emacs-directory)))
      (yas-load-directory snippets-dir)
      (setq-default yas/snippet-dirs snippets-dir))
    (yas-global-mode 1)
    (setq-default yas/prompt-functions '(yas/ido-prompt))))

;; ============================================================
;; Text Manipulation
;; ============================================================


;; ------------------------------ delete word
;; (defun rangi/delete-word (arg)
;;   "Delete word after cursor without add to kill ring"
;;   (interactive "p")
;;   (delete-region (point)
;;                  (progn
;;                    (if (rangi/minor-mode-on-p 'subword-mode)
;;                        (subword-forward arg)
;;                        (forward-word arg))
;;                     (point))))

;; (defun rangi/backward-delete-word (arg)
;;   "Delete word before cursor without add it to kill ring"
;;   (interactive "p")
;;   (rangi/delete-word (- arg)))

;; (bind-key "C-M-h" 'rangi/backward-delete-word)
;; (bind-key "M-d" 'rangi/delete-word)


(bind-key "C-d" 'delete-forward-char)
(bind-key "C-M-h" 'backward-kill-word)
;; ------------------------------ delete character backward
(define-key key-translation-map (kbd "C-h") (kbd "DEL"))

;; ------------------------------ move text
(defun rangi/move-text-up (arg)
  "Move text up, but recenter if at upper part of the window"
  (interactive "*p")
  (move-text-up arg)
  (if (rangi--point-is-at-upper-window)
      (recenter-top-bottom (truncate (/ (window-text-height) 2)))))

(defun rangi/move-text-down (arg)
  "Move text up, but recenter if at lower part of the window"
  (interactive "*p")
  (move-text-down arg)
  (if (region-active-p)
      (exchange-point-and-mark))
  (if (rangi--point-is-at-lower-window)
      (recenter-top-bottom (truncate (/ (window-text-height) 2)))))

(defun rangi--point-is-at-upper-window ()
  "Check point is at upper part of current window"
  (<= (cdr (posn-col-row (posn-at-point))) (truncate (/ (window-text-height) 2))))

(defun rangi--point-is-at-lower-window ()
  "Check point is at lower part of current window"
  (> (cdr (posn-col-row (posn-at-point))) (truncate (/ (window-text-height) 2))))

(bind-key "M-P" 'rangi/move-text-up)
(bind-key "M-N" 'rangi/move-text-down)

;; ------------------------------ zapzapzapzap
(use-package misc
  :bind (("M-Z" . zap-to-char)
         ("M-z" . zap-up-to-char)))

;; ------------------------------ duplicator
(use-package duplicator
  :load-path "site-lisp/duplicator"
  :bind ("C-c d" . duplicator/duplicate-lines))

;; ------------------------------ insert new line
(defun rangi/insert-newline-above ()
  "Insert a newline above the current line."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun rangi/insert-newline-below ()
  "Insert a newline below the current line."
  (interactive)
  (end-of-line)
  (newline)
  (indent-according-to-mode))

(bind-key "<M-S-return>" 'rangi/insert-newline-above)
(bind-key "<S-return>" 'rangi/insert-newline-below)

;; ------------------------------ comment
;; (defun rangi/comment-or-uncomment-region-or-line ()
;;   "Comments or uncomments the region or the current line if there's no active region."
;;   (interactive)
;;   (let (beg end)
;;     (if (region-active-p)
;;         (setq beg (region-beginning) end (region-end))
;;       (setq beg (line-beginning-position) end (line-end-position))
;;       (deactivate-mark))
;;     (comment-or-uncomment-region beg end)
;;     (next-logical-line)))

(bind-key "C-/" 'comment-dwim)

;; ------------------------------ shuffle lines
(use-package randomize-region
  :load-path "site-lisp/randomize-region")

;; ------------------------------ transpose by delimiter
(defun rangi/transpose-by-delimiter (delimiter)
  (interactive "sTranspose by delimiter: ")
    (query-replace-regexp
     (format "\\(.*?\\)%s\\(.*\\)" delimiter)
     (format "\\2%s\\1" delimiter)
     nil (region-beginning) (region-end)))

(bind-key "M-T" 'rangi/transpose-by-delimiter)

;; ------------------------------ smartparens
(use-package smartparens
  :init
  (progn
    (use-package smartparens-config)
    (use-package smartparens-ruby)
    (use-package smartparens-html)
    (smartparens-global-mode 1)
    (show-smartparens-global-mode 1))
  :config
  (progn
    (setq smartparens-strict-mode t)
    (setq sp-autoescape-string-quote nil)
    (setq sp-autoinsert-if-followed-by-word t)

    (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))

    (sp-local-pair 'web-mode "<?php " " ?>" :trigger "<?p"))

    (sp-local-pair 'php-mode "<?php " " ?>" :trigger "<?p")

  :bind
  (("C-M-k" . sp-kill-sexp)
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-n" . sp-up-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-u" . sp-backward-up-sexp)
   ("C-M-p" . sp-backward-down-sexp)
   ("C-M-w" . sp-copy-sexp)
   ("M-s" . sp-splice-sexp)
   ("M-r" . sp-splice-sexp-killing-around)
   ("C-)" . sp-forward-slurp-sexp)
   ("C-}" . sp-forward-barf-sexp)
   ("C-(" . sp-backward-slurp-sexp)
   ("C-{" . sp-backward-barf-sexp)
   ("M-S" . sp-split-sexp)
   ("M-J" . sp-join-sexp)
   ("C-M-t" . sp-transpose-sexp)))

(provide 'setup-editing)
