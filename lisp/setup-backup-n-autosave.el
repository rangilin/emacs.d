(require 'functions)

;; ------------------------------ backup
(setq-default backup-by-copying t)

;; put backup files in gen dir
(let ((rl/backup-dir (rl/sub-gen-dir "backup")))
  (setq backup-directory-alist `((".*" . ,rl/backup-dir))))

;; ------------------------------ auto-save
;; put auto save files in gen dir
(let ((rl/autosave-dir (rl/sub-gen-dir "autosave")))
  (setq auto-save-file-name-transforms `((".*" ,rl/autosave-dir t)))
  ;; prevent `auto-save-list' empty dir created
  (setq auto-save-list-file-prefix nil))

(provide 'setup-backup-n-autosave)
