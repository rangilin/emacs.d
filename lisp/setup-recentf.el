(require 'use-package)

(use-package recentf
  :init
  (progn
    (setq-default recentf-exclude
                  `("COMMIT_EDITMSG"
                    "/Dropbox/journal"
                    "\\.elc$"))
    (setq-default recentf-save-file (expand-file-name "recentf" rangi/gen-dir))
    (setq-default recentf-max-saved-items 100)
    (recentf-mode 1))
  :bind (("C-x C-r" . rangi/recentf-ido-find-file)
         ("C-x 4 r" . rangi/recentf-ido-find-file-other-window)))

(defun rangi/recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (rangi--recentf-ido 'find-file))

(defun rangi/recentf-ido-find-file-other-window ()
  "Find a recent file using ido and open it at other window"
  (interactive)
  (rangi--recentf-ido 'find-file-other-window))

(defun rangi--recentf-ido (find-file-function)
  "Using ido to find recent file with specified function"
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (if (file-writable-p file)
          (funcall find-file-function file)
        (funcall find-file-function (concat "/sudo::" file))))))

(provide 'setup-recentf)
