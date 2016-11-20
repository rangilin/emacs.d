(require 'use-package)
(require 'functions)

(setq-default tab-width 4)
(setq-default comment-empty-lines t)
(setq-default indent-tabs-mode nil)
(setq-default require-final-newline t)

(setq-default mouse-yank-at-point t)
(setq-default x-select-enable-clipboard t)
(setq-default save-interprogram-paste-before-kill t)

(delete-selection-mode 1)
(show-paren-mode 1)

(setq-default default-fill-column 80)

(bind-key "M-\\" 'cycle-spacing)
(bind-key "M-Q" 'fill-region)
(bind-key "RET" 'newline-and-indent)

;; ------------------------------ expand words

(bind-key "M-/" (make-hippie-expand-function
                 '(try-expand-dabbrev-visible
                   try-expand-dabbrev
                   try-expand-dabbrev-all-buffers
                   try-complete-file-name-partially
                   try-complete-file-name
                   try-expand-list
                   try-expand-line
                   try-expand-dabbrev-from-kill
                   try-complete-lisp-symbol-partially
                   try-complete-lisp-symbol
                   ) t))

;; ------------------------------ join line
(defun rangi-join-below-line ()
  (interactive)
  (join-line 1))

(bind-key "C-j" 'rangi-join-below-line)
(bind-key "C-S-j" 'join-line)

;; ------------------------------ browse kill ring
(use-package browse-kill-ring
  :bind (("C-S-y" . browse-kill-ring)))

;; ------------------------------ expand region
(use-package expand-region
  :bind (("C-'" . er/expand-region)
         ("C-\"" . er/contract-region)))

;; ------------------------------ change inner
(use-package change-inner
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

;; ------------------------------ undo
(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode 1)
  :config
  (bind-key "C-/" nil undo-tree-map)
  (bind-key "C-?" nil undo-tree-map))

;; ------------------------------ yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (setq yas-prompt-functions '(yas-ido-prompt))
  (setq yas-snippet-dirs (concat (file-name-as-directory user-emacs-directory) "snippets"))
  (yas-global-mode 1))

;; ============================================================
;; Text Manipulation
;; ============================================================

;; ------------------------------ case
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; ------------------------------ delete word
;; (defun rangi-delete-word (arg)
;;   "Delete word after cursor without add to kill ring"
;;   (interactive "p")
;;   (delete-region (point)
;;                  (progn
;;                    (if (rangi-minor-mode-on-p 'subword-mode)
;;                        (subword-forward arg)
;;                        (forward-word arg))
;;                     (point))))

;; (defun rangi-backward-delete-word (arg)
;;   "Delete word before cursor without add it to kill ring"
;;   (interactive "p")
;;   (rangi-delete-word (- arg)))

;; (bind-key "C-M-h" 'rangi-backward-delete-word)
;; (bind-key "M-d" 'rangi-delete-word)


(bind-key "C-d" 'delete-forward-char)
(bind-key "C-M-h" 'backward-kill-word)
;; ------------------------------ delete character backward
(define-key key-translation-map (kbd "C-h") (kbd "DEL"))

;; ------------------------------ move text
(bind-key "M-P" 'move-text-up)
(bind-key "M-N" 'move-text-down)

;; ------------------------------ zapzapzapzap
(use-package misc
  :bind (("M-z" . zap-to-char)
         ("M-Z" . zap-up-to-char)))

;; ------------------------------ duplicator
(use-package duplicator
  :load-path "site-lisp/duplicator"
  :bind ("C-c d" . duplicator/duplicate-lines))

;; ------------------------------ insert new line
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

(bind-key "<M-S-return>" 'rangi-insert-newline-above)
(bind-key "<S-return>" 'rangi-insert-newline-below)

;; ------------------------------ comment
;; (defun rangi-comment-or-uncomment-region-or-line ()
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
(defun rangi-transpose-by-delimiter (delimiter)
  (interactive "sTranspose by delimiter: ")
    (query-replace-regexp
     (format "\\(.*?\\)%s\\(.*\\)" delimiter)
     (format "\\2%s\\1" delimiter)
     nil (region-beginning) (region-end)))

(bind-key "M-T" 'rangi-transpose-by-delimiter)

;; ------------------------------ smartparens
(use-package smartparens
  :diminish ""
  :init
  (use-package smartparens-config)
  (use-package smartparens-ruby)
  (use-package smartparens-html)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)

  :config
  (setq smartparens-strict-mode t)
  (setq-default sp-autoinsert-if-followed-by-word t)
  (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))
  (sp-local-pair 'web-mode "<?php " " ?>" :trigger "<?p")
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
