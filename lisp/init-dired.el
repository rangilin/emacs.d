(require 'dired)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

;; sort dired list
(setq dired-listing-switches "-aBhl  --group-directories-first")

(provide 'init-dired)
