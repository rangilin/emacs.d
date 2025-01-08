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
  (setq-default transient-levels-file (expand-file-name "transient-levels.el" rangi-emacs-cache-directory))
  (setq-default transient-values-file (expand-file-name "transient-values.el" rangi-emacs-cache-directory))
  (setq-default transient-history-file (expand-file-name "transient-history.el" rangi-emacs-cache-directory))

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
