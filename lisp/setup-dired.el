(require 'use-package)

(use-package dired
  :init
  (setq dired-listing-switches "-aBhl  --group-directories-first")
  (setq dired-dwim-target t)

;;   (put 'dired-find-alternate-file 'disabled nil)
;;   (defun rangi/dired-up-directory (prefix)
;;     "My `dired-up-directory' alternate which open parent directory in current
;; buffer by default. However, open in new buffer if prefix is given"
;;     (interactive "p")
;;     (if current-prefix-arg
;;         (dired-up-directory)
;;       (find-alternate-file "..")))
;;
;;   (defun rangi/dired-find-file (prefix)
;;     "My `dired-find-file' that open directory in current buffer, open in new
;; buffer when prefix is given"
;;     (interactive "P")
;;     (if current-prefix-arg
;;         (dired-find-file)
;;       (dired-find-alternate-file)))
;;
;;   (bind-key "^" 'rangi/dired-up-directory dired-mode-map)
;;   (bind-key "RET" 'rangi/dired-find-file dired-mode-map)
  )

(provide 'setup-dired)
