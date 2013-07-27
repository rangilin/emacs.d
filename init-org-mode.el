(add-to-list 'load-path (expand-file-name "vendor/org/lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "vendor/org/contrib/lisp" user-emacs-directory))

(setq-default org-catch-invisible-edits t)


(provide `init-org-mode)
