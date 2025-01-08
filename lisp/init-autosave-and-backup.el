;;
;; Auto save
;; ----------------------------------------------------------------------------
;;

;; set up autosave directory
(setq rangi-autosave-directory (expand-file-name "autosave" rangi-emacs-cache-directory))
(unless (file-exists-p rangi-autosave-directory)
  (make-directory rangi-autosave-directory))

;; put auto save files in to autosave dir
(setq auto-save-file-name-transforms `((".*" ,rangi-autosave-directory t)))

;; prevent `auto-save-list' empty dir created
(setq auto-save-list-file-prefix nil)



;;
;; Backup
;; ----------------------------------------------------------------------------
;;

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
(setq kept-old-versions 3)
(setq kept-new-versions 5)

;; delete old version of backup automatically
(setq delete-old-versions t)


(provide 'init-autosave-and-backup)
