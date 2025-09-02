;;; init-file.el --- File related configurations -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous file settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; move file to trash when deleted
(setq-default delete-by-moving-to-trash t)



;;;;;;;;;;;;;;;
;; Auto Save ;;
;;;;;;;;;;;;;;;

;; prevent `auto-save-list' empty dir created
(setq auto-save-list-file-prefix nil)

;; set up autosave directory
(setq rangi-auto-save-directory (expand-file-name "auto-save" rangi-emacs-cache-directory))
(unless (file-exists-p rangi-auto-save-directory)
  (make-directory rangi-auto-save-directory))

;; put auto save files in to autosave dir
(setq auto-save-file-name-transforms `((".*" ,rangi-auto-save-directory t)))



;;;;;;;;;;;;
;; Backup ;;
;;;;;;;;;;;;

;; set up backup directory
(setq rangi-backup-directory (expand-file-name "backup" rangi-emacs-cache-directory))
(unless (file-exists-p rangi-backup-directory)
  (make-directory rangi-backup-directory))

;; put backup files into backup directory
(setq backup-directory-alist `((".*" . ,rangi-backup-directory)))

;; use copy to backup files
(setq backup-by-copying t)
;; number version backup files
(setq version-control t)
;; kept no old backups
(setq kept-old-versions 0)
;; kept at most this many backups
(setq kept-new-versions 5)
;; delete old version of backup automatically
(setq delete-old-versions t)



;;;;;;;;;;;
;; Dired ;;
;;;;;;;;;;;

(use-package dired
  :config
  ;; use GNU ls
  (setq insert-directory-program "gls")
  ;; dired list options
  (setq dired-listing-switches "-ahBp --group-directories-first")
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
  :demand t
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
