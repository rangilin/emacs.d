;; tab in source blocks should act like in major mode
(setq org-src-tab-acts-natively t)

;; code highlight in source blocks
(setq org-src-fontify-natively t)

;; store logs in drawer
(setq org-log-into-drawer t)

;; opened org file started show only overview
(setq org-startup-folded t)

;; better behaviors for these key
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-special-ctrl-o t)

;; never add blank line on heading and list
(setq org-blank-before-new-entry
      '((heading . nil) (plain-list-item . nil)))

;; prevent edit hidden text accidentally
(setq org-ctrl-k-protect-subtree t)
(setq org-catch-invisible-edits 'show-and-error)

;; indent to make document more clearly
(setq org-startup-indented t)

;; open link when press return on the link
(setq org-return-follows-link t)

;; force todo/checkbox dependency
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)

;; allow entering a note upon closing a task
(setq org-log-done 'note)

;; don't allow edit inivisbe characters
(setq org-catch-invisible-edits 'error)

;; make shift selection works in org mode navigation commands
(defadvice org-beginning-of-line
    (before advice-org-beginning-of-line activate)
  (handle-shift-selection))
(defadvice org-end-of-line
    (before advice-org-end-of-line activate)
  (handle-shift-selection))
(defadvice org-table-beginning-of-field
    (before advice-org-table-beginning-of-field activate)
  (handle-shift-selection))
(defadvice org-table-end-of-field
    (before advice-org-table-end-of-field activate)
  (handle-shift-selection))
(setq org-support-shift-select nil)


;; remove some keybinding in org mode
(define-key org-mode-map (kbd "C-c C-e") nil)


(use-package org-bullets
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode))


(provide 'init-org)
