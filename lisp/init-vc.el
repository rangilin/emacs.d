(setq vc-handled-backends `(SVN Git))

;;
;; Git
;; ----------------------------------------------------------------------------
;;

(use-package magit
  :bind
  (("C-c v g" . magit-status)
   ("C-c v G" . magit-dispatch-popup))

  :init
  (setq-default transient-levels-file (locate-user-emacs-file (convert-standard-filename "gen/transient/levels.el")))
  (setq-default transient-values-file (locate-user-emacs-file (convert-standard-filename "gen/transient/values.el")))
  (setq-default transient-history-file (locate-user-emacs-file (convert-standard-filename "gen/transient/history.el")))

  :config
  (setq-default magit-diff-refine-hunk t))



;;
;; Subversion
;; ----------------------------------------------------------------------------
;;

(use-package dsvn
  :bind
  (("C-c v s" . svn-status)))


(provide 'init-vc)
