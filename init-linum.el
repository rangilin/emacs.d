;; http://www.emacswiki.org/emacs/linum-off.el

(require 'linum)

(defcustom linum-disabled-modes '(eshell-mode text-mode dired-mode)
  "* List of modes disabled when global linum mode is on"
  :type '(repeat (sexp :tag "Major mode"))
  :tag " Major modes where linum is disabled: "
  :group 'linum
  )

(defun linum-on ()
  (unless (or (minibufferp)
              (member major-mode linum-disabled-modes)
              (string-match "*" (buffer-name)))
    (linum-mode 1)))


(global-linum-mode 1)


(provide 'init-linum)
