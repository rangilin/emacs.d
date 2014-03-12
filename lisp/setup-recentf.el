(require 'use-package)

(use-package recentf
  :init
  (progn
    (setq-default recentf-exclude `("COMMIT_EDITMSG"))
    (setq-default recentf-save-file (expand-file-name "recentf" rl/gen-dir))
    (setq-default recentf-max-saved-items 100)
    (recentf-mode 1))
  :bind (("C-x C-r" . rl/recentf-ido-find-file)
         ("C-x 4 r" . rl/recentf-ido-find-file-other-window)))

(defun rl/recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (rl--recentf-ido 'find-file))

(defun rl/recentf-ido-find-file-other-window ()
  "Find a recent file using ido and open it at other window"
  (interactive)
  (rl--recentf-ido 'find-file-other-window))

(defun rl--recentf-ido (find-file-function)
  "Using ido to find recent file with specified function"
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (funcall find-file-function file))))

(provide 'setup-recentf)
