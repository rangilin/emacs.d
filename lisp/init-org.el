;; better behaviors for these key
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-special-ctrl-o t)


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


(provide 'init-org)
