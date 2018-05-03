;;;; Refresh buffer

;; press f5 to refresh buffer
(defun rangi-refresh-buffer ()
  "Referesh current buffer."
  (interactive)
  (revert-buffer nil t nil)
  (message "buffer is refreshed"))

(global-set-key (kbd "<f5>") 'rangi-refresh-buffer)




;;;; ibuffer

(require 'ibuffer)

;; set up how buffers is grouped in the ibuffer
(setq-default ibuffer-saved-filter-groups
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


;; but don't show empty group
(setq-default ibuffer-show-empty-filter-groups nil)


;; switch group on active
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))


;; define a ibuffer column that show human readable size of the buffer
(define-ibuffer-column readable-size
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 1000) (format "%7.1fK" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))


;; define ibuffer columns
(setq-default ibuffer-formats
              '((mark modified read-only
                      " " (name 36 36 :left :elide)
                      " " (readable-size 9 -1 :right)
                      " " (mode 16 16 :left :elide)
                      " " filename-and-process)))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'init-buffer)
