(require 'use-package)

(use-package recentf
  :init
  (progn
    (recentf-mode 1))
  :bind ("C-x C-r" . rl/recentf-ido-find-file))

(defun rl/recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))
(provide 'setup-recentf)
