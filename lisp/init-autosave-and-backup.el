;;;; Backup

;; set up backup directory
(defconst rangi-backup-directory (expand-file-name "backup" rangi-generated-files-directory))
(unless (file-exists-p rangi-backup-directory)
  (make-directory rangi-backup-directory))

;; put backup files into backup directory
(setq backup-directory-alist `((".*" . ,rangi-backup-directory)))

;; use copy to backup file
(setq backup-by-copying t)




;;;; Auto saves

;; set up autosave directory
(defconst rangi-autosave-directory (expand-file-name "autosave" rangi-generated-files-directory))
(unless (file-exists-p rangi-autosave-directory)
  (make-directory rangi-autosave-directory))

;; put auto save files in to autosave dir
(setq auto-save-file-name-transforms `((".*" ,rangi-autosave-directory t)))

;; prevent `auto-save-list' empty dir created
(setq auto-save-list-file-prefix nil)


(provide 'init-autosave-and-backup)
