(require 'functions)

;; ------------------------------ backup
(setq-default backup-by-copying t)

;; put backup files in gen dir
(let ((dir (rangi-sub-gen-dir "backup")))
  (setq backup-directory-alist `((".*" . ,dir))))

;; ------------------------------ auto-save
;; put auto save files in gen dir
(let ((dir (rangi-sub-gen-dir "autosave")))
  (setq auto-save-file-name-transforms `((".*" ,dir t))))

;; prevent `auto-save-list' empty dir created
(setq auto-save-list-file-prefix nil)

(provide 'setup-backup-n-autosave)
