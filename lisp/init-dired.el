(require 'dired)

;; smart target
(setq dired-dwim-target t)

;; open current folder
(global-set-key (kbd "<f12>") 'dired-jump-other-window)

;; open home folder
(global-set-key (kbd "<S-f12>") (lambda () (interactive) (dired-other-window "~/")))

;; sort dired list
(setq dired-listing-switches "-alhBF")

;; don't ask about killing buffer visiting file
(setq dired-clean-confirm-killing-deleted-buffers t)

;; stop asking about recurisve actions
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)



(provide 'init-dired)
