(require 'linum)

(defcustom linum-disabled-modes '(eshell-mode text-mode dired-mode))

(defun linum-on ()
  (unless (or (minibufferp)
              (member major-mode linum-disabled-modes)
              (string-match "*" (buffer-name)))
    (linum-mode 1)))


(global-linum-mode 1)


(provide 'init-linum)
