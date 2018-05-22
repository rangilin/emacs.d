(require 'dired)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

;; sort dired list
(setq dired-listing-switches "-aBhl  --group-directories-first")

;; smart target
(setq dired-dwim-target t)

;; open current folder
(global-set-key (kbd "<f12>") 'dired-jump-other-window)
;; open home folder
(global-set-key (kbd "<S-f12>") (lambda () (interactive) (dired-other-window "~/")))

(provide 'init-dired)
