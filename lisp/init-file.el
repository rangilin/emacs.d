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



;;;;;;;;;;;;;;;;;;
;; Recent Files ;;
;;;;;;;;;;;;;;;;;;

(use-package recentf
  :bind (("C-c f r" . recentf-open)
	 ("C-c f R" . recentf-open-files))
  :config
  ;; put recentf file in designated cache directory
  (setq recentf-save-file (expand-file-name "recentf" rangi-emacs-cache-directory))

  ;; store more items
  (setq recentf-max-menu-items 30)

  ;; exlucde these entries
  (setq recentf-exclude `("/ssh:"))

  ;; add opened file into recentf when buffer list change
  (add-hook 'buffer-list-update-hook #'recentf-track-opened-file)
  (recentf-mode 1))


(provide 'init-file)
