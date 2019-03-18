(require-package 'magit)

(setq-default transient-levels-file (locate-user-emacs-file (convert-standard-filename "gen/transient/levels.el")))
(setq-default transient-values-file (locate-user-emacs-file (convert-standard-filename "gen/transient/values.el")))
(setq-default transient-history-file (locate-user-emacs-file (convert-standard-filename "gen/transient/history.el")))

(with-eval-after-load 'magit
  (setq-default magit-diff-refine-hunk t))

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x G") 'magit-dispatch-popup)



(provide 'init-vc)
