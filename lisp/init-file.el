;;; init-file.el --- File related configurations -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous file settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; move file to trash when deleted
(setq-default delete-by-moving-to-trash t)


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
  ;; always yes
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
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



;;;;;;;;;;;;
;; Denote ;;
;;;;;;;;;;;;

(use-package denote
  :pin gnu
  :ensure t
  :hook ((dired-mode . denote-dired-mode)
         (text-mode . denote-fontify-links-mode-maybe))
  :bind
  (("C-c n n" . denote)
   ("C-c n r" . denote-rename-file)
   ("C-c n l" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-dired)
   ("C-c n g" . denote-grep))
  :config
  (setq denote-directory (expand-file-name "~/Documents/notes/"))
  (denote-rename-buffer-mode 1))



(provide 'init-file)
