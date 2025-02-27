;;; init-buffer.el --- Buffer stuff  -*- lexical-binding: t -*-

;; quickly change buffers
(bind-key "s-<" 'previous-buffer)
(bind-key "s->" 'next-buffer)


;;
;; New buffer
;; ----------------------------------------------------------------------------
;;

;; new buffer start with text mode
(setq-default major-mode 'text-mode)

;; helper function for create new buffer with assigned start major mode
(defun rangi-new-buffer ()
  (interactive)
  (let ((buffer (generate-new-buffer (read-string "Enter buffer name: " "*scratch*"))))
    (set-buffer-major-mode buffer)
    (switch-to-buffer buffer)))

(bind-key "M-s-b" 'rangi-new-buffer)


;;
;; Revert buffer
;; ----------------------------------------------------------------------------
;;

;; refresh buffer
(defun rangi-refresh-buffer ()
  "Referesh current buffer."
  (interactive)
  (revert-buffer nil t nil)
  (message "buffer is refreshed"))

(bind-key "<f5>" 'rangi-refresh-buffer)



;; auto refresh buffer
(use-package autorevert
  :delight auto-revert-mode
  :config
  (global-auto-revert-mode 1)

  ;; don't refresh non-file buffer
  (setq global-auto-revert-non-file-buffers t)

  ;; don't print anything after refresh
  (setq auto-revert-verbose nil))


;;
;; ibuffer
;; ----------------------------------------------------------------------------
;;

(use-package ibuffer
  :config
  (use-package ibuffer-vc)
  (use-package ibuffer-tramp)

  ;; don't show empty group
  (setq-default ibuffer-show-empty-filter-groups nil)

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
			" " (name 60 60 :left :elide)
			" " (readable-size 9 -1 :right)
			" " (mode 16 16 :left :elide)
			" " filename-and-process)))

  (defun rangi-switch-ibuffer-filter-groups (arg)
    (interactive "p")
    (message "Group by: (m): major mode (v): version control (t): tramp connection")
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "m") 'ibuffer-set-filter-groups-by-mode)
       (define-key map (kbd "v") 'ibuffer-vc-set-filter-groups-by-vc-root)
       (define-key map (kbd "t") 'ibuffer-tramp-set-filter-groups-by-tramp-connection)
       map)
     t))

  (bind-key "s-B" 'ibuffer)
  (bind-key "C-x C-b" 'ibuffer)
  (unbind-key "P" ibuffer-mode-map) ; unbind print function so I don't accidentally print all of my buffers
  (bind-key "C-c m s" 'rangi-switch-ibuffer-filter-groups ibuffer-mode-map))



(use-package buffer-move
  :config
  (setq buffer-move-stay-after-swap t))


(provide 'init-buffer)
