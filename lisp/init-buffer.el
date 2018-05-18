;;;; Refresh buffer

;; press f5 to refresh buffer
(defun rangi-refresh-buffer ()
  "Referesh current buffer."
  (interactive)
  (revert-buffer nil t nil)
  (message "buffer is refreshed"))

(global-set-key (kbd "<f5>") 'rangi-refresh-buffer)




;;;; ibuffer

(require-package 'ibuffer-vc)
(require-package 'ibuffer-tramp)

(require 'ibuffer)

;; don't show empty group
(setq-default ibuffer-show-empty-filter-groups nil)


;; switch group on active
(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-vc-set-filter-groups-by-vc-root)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

;; define a ibuffer column that show human readable size of the buffer
(define-ibuffer-column readable-size
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 1000) (format "%7.1fK" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

;; define ibuffer columns
(setq-default ibuffer-formats
              '((mark modified read-only vc-status-mini
                      " " (name 36 36 :left :elide)
                      " " (readable-size 9 -1 :right)
                      " " (mode 16 16 :left :elide)
                      " " filename-and-process)))

(defun rangi-switch-ibuffer-filter-groups (arg)
  (interactive "p")
  (message "Group by: (m): major mode (v): version control")
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "m") 'ibuffer-set-filter-groups-by-mode)
     (define-key map (kbd "v") 'ibuffer-vc-set-filter-groups-by-vc-root)
     map)
   t))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(define-key ibuffer-mode-map (kbd "C-c m s") 'rangi-switch-ibuffer-filter-groups)




;;;; auto revert
(global-auto-revert-mode 1)
(diminish 'auto-revert-mode)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)




;;;; use text mode as default major mode of new buffer
(setq-default major-mode 'text-mode)
(setq initial-major-mode 'text-mode)




;;;; switch buffer
(global-set-key (kbd "C-<tab>") 'mode-line-other-buffer)




(provide 'init-buffer)
