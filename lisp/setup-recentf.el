(require 'use-package)

(use-package recentf
  :init
  (progn
    (setq-default recentf-exclude `("COMMIT_EDITMSG"))
    (setq-default recentf-save-file (expand-file-name "recentf" rl/gen-dir))
    (setq-default recentf-max-saved-items 100)
    (recentf-mode 1))
  :bind ("C-x C-r" . rl/recentf-ido-find-file))

(defun rl/recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))
(provide 'setup-recentf)
