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


;; unset M-h
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-h") nil))


(provide 'init-org)
