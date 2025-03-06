;;; init-file.el --- File related configurations -*- lexical-binding: t -*-

;;;;;;;;;;;
;; Dired ;;
;;;;;;;;;;;

(use-package dired
  :config
  ;; change dired list options so it show readable size and show non-printable characters
  (setq dired-listing-switches "-alhB")
  ;; enable find alternate file command
  (put 'dired-find-alternate-file 'disabled nil)
  ;; only ask for top-level directories when delete recursively
  (setq dired-recursive-deletes 'top)
  ;; smart target
  (setq dired-dwim-target t))



;;;;;;;;;;;;
;; Search ;;
;;;;;;;;;;;;

;; editing grep result on grep buffer
(use-package wgrep
  :ensure t
  :pin nongnu)



(provide 'init-file)
