(require-package 'magit)

(with-eval-after-load 'magit
  (setq-default magit-diff-refine-hunk t))

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x G") 'magit-dispatch-popup)



(provide 'init-vc)
