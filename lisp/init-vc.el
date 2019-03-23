;;
;; Git
;; ----------------------------------------------------------------------------
;;

(use-package magit
  :bind
  (("s-g" . magit-status)
   ("s-G" . magit-dispatch-popup))

  :init
  (setq-default transient-levels-file (locate-user-emacs-file (convert-standard-filename "gen/transient/levels.el")))
  (setq-default transient-values-file (locate-user-emacs-file (convert-standard-filename "gen/transient/values.el")))
  (setq-default transient-history-file (locate-user-emacs-file (convert-standard-filename "gen/transient/history.el")))

  :config
  (setq-default magit-diff-refine-hunk t))



(provide 'init-vc)
