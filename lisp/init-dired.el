(require 'dired)

;; smart target
(setq dired-dwim-target t)

;; open current folder
(global-set-key (kbd "<f12>") 'dired-jump-other-window)

;; open home folder
(global-set-key (kbd "<S-f12>") (lambda () (interactive) (dired-other-window "~/")))


;; Prefer g-prefixed coreutils version of standard utilities when available
(setq insert-directory-program (executable-find "gls"))

;; sort dired list
(setq dired-listing-switches "-aBhl  --group-directories-first")


(provide 'init-dired)
