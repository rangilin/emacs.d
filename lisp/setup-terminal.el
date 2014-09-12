(require 'use-package)
;; ------------------------------ term
(use-package multi-term
  :bind (("C-c t" . multi-term))
  :config
  (progn

    ;; ------------------------------ workaround view-lossage issues
    ;; clear recorded keystroke on enter is pressed
    ;; avoid view-lossage to display password
    (defadvice term-send-raw (after clear-recorded-key activate)
      (if (and (stringp (this-command-keys))
               (string= (kbd "RET") (this-command-keys)))
          (clear-this-command-keys)))

    ;; ------------------------------ hook for setup term mode
    (defun rangi/setup-term-mode ()
      (yas-minor-mode -1)) ; yas tab not works well with term
    (add-hook 'term-mode-hook 'rangi/setup-term-mode)

    (setq-default multi-term-program "/bin/bash")
    (setq-default multi-term-program-switches "--login")
    (setq-default term-buffer-maximum-size 10000)
    (setq-default multi-term-switch-after-close nil)
    (setq-default term-unbind-key-list
                  `(
                    "C-x" ;; all command with prefix "C-x"
                    "M-x" ;; smex
                    "M-[" ;; previous-buffer
                    "M-]" ;; next-buffer
                    "C-M-v" ;; other window page down
                    "C-M-V" ;; other window page up
                    ))
    (setq-default term-bind-key-alist
                  '(
                    ("C-c C-c" . term-interrupt-subjob)
                    ("C-c C-j" . term-line-mode)
                    ("C-c C-k" . term-char-mode)
                    ("C-m" . term-send-raw)
                    ("M-v" . term-send-raw)
                    ("M-/" . term-send-raw)
                    ("M-f" . term-send-forward-word)
                    ("M-b" . term-send-backward-word)
                    ("M-d" . term-send-forward-kill-word)
                    ("C-M-h" . term-send-backward-kill-word)
                    ("C-y" . term-paste)))

    ;; ------------------------------ workaround directory with Chinese name bug
    (defadvice term-command-hook (before decode-string)
      (setq string (decode-coding-string string locale-coding-system)))
    (when (version< emacs-version "24.3.50.1") (ad-activate 'term-command-hook))

    ))

;; ------------------------------ exec path
(use-package exec-path-from-shell
  :init
  (progn
    (setq-default exec-path-from-shell-variables
                  '("PATH" "MANPATH" "GOROOT" "GOPATH"))
    (exec-path-from-shell-initialize)))

;; ------------------------------ comint
(use-package comint
  :init
  (setq-default comint-scroll-to-bottom-on-output 'all))

(provide 'setup-terminal)
