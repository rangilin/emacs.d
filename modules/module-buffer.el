;; -------------------------------------------------------------------
;;
;; ibuffer
;;
;; -------------------------------------------------------------------

(require 'ibuffer)

(defvar rl--ibuffer-filter-groups
  `(("default"
     ("Dired" (mode . dired-mode))
     ("Emacs" (or (name . "\*Messages\*")
                  (name . "\*Warnings\*")
                  (name . "\*Completions\*")
                  (name . "\*Compile-Log\*")
                  (name . "\*Backtrace\*")))
     ("Help" (or (mode . man-mode)
                 (mode . woman-mode)
                 (mode . info-mode)
                 (mode . help-mode)))
     ("Org" (mode . org-mode))
     ("SQL client" (mode . sql-interactive-mode))
     ("Terminal" (or (mode . term-mode)
                     (mode . shell-mode)
                     (mode . eshell-mode)))
     ("Temporary" (name . "\*.*\*")))))


(defvar rl--ibuffer-format
  '((mark modified read-only
          " " (name 24 24 :left :elide)
          " " (readable-size 9 -1 :right)
          " " (mode 16 16 :left :elide)
          " " filename-and-process)))

(setq-default ibuffer-show-empty-filter-groups nil)
(setq-default ibuffer-saved-filter-groups rl--ibuffer-filter-groups)
(setq-default ibuffer-formats rl--ibuffer-format)


(define-ibuffer-column readable-size
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 1000) (format "%7.1fK" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(bind-key "C-x C-b" 'ibuffer)


;; -------------------------------------------------------------------
;;
;; auto revert
;;
;; -------------------------------------------------------------------


(defun rl-refresh-buffer ()
  "Referesh current buffer."
  (interactive)
  (revert-buffer nil t nil)
  (message "buffer is refreshed"))

(bind-key "<f5>" 'rl-refresh-buffer)

(setq-default auto-revert-verbose nil)
(setq-default global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)


;; -------------------------------------------------------------------
;;
;; scratch buffer
;;
;; -------------------------------------------------------------------


(setq initial-major-mode 'text-mode)





(provide 'module-buffer)
