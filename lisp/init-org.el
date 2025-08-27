;;; init-org.el --- Org mode related configurations -*- lexical-binding: t -*-

;; opened org file started show only overview
(setq org-startup-folded t)

;; indent to make document more clearly
(setq org-startup-indented t)

;; better behaviors for these key
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-special-ctrl-o t)

;; prevent edit hidden text accidentally
(setq org-ctrl-k-protect-subtree t)
(setq org-catch-invisible-edits 'show-and-error)

;; force todo/checkbox dependency
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)

;; don't insert empty line when creating new-entry
(setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))


(provide 'init-org)
