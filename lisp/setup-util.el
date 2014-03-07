(require 'use-package)

;; ------------------------------ comment
(defun rl/comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position))
      (deactivate-mark))
    (comment-or-uncomment-region beg end)
    (next-logical-line)))

(bind-key "C-/" 'rl/comment-or-uncomment-region-or-line)

;; ------------------------------ byte compile
(defun rl/byte-recompile ()
  "Byte-compile init files to improve speed"
  (interactive)
  (byte-recompile-directory "~/.emacs.d" 0))

(bind-key "C-c <f12>" 'rl/byte-recompile)

;; ------------------------------ trailing whitespace
(dolist (hook '(eshell-mode-hook
		shell-mode-hook
		diff-mode-hook
		comint-mode-hook
		term-mode-hook))
  (add-hook hook (lambda () (setq show-trailing-whitespace nil))))

(defun turn-off-whitespace-mode-by-file-extension ()
  (when (and (stringp buffer-file-name)
             (string-match "\\.log" buffer-file-name))
    (setq show-trailing-whitespace nil)))

(add-hook 'find-file-hook 'turn-off-whitespace-mode-by-file-extension)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ------------------------------ shuffle lines
(use-package randomize-region
  :load-path "site-lisp/randomize-region")

;; ------------------------------ smex
(use-package smex
  :bind ("M-x" . smex))

;; ------------------------------ projectile
(use-package projectile
  :diminish projectile-mode
  :init
  (progn
    (projectile-global-mode)
    (setq-default projectile-switch-project-action 'projectile-dired)
    (setq-default projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" rl/gen-dir))
    (projectile-load-known-projects)))

;; ------------------------------ apropos
;; use apropos instead of apropos-command
(bind-key "a" 'apropos help-map)

(provide 'setup-util)
