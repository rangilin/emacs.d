(global-set-key (kbd "s-<") 'previous-buffer)
(global-set-key (kbd "s->") 'next-buffer)

(defun rangi-new-buffer ()
  (interactive)
  (switch-to-buffer (generate-new-buffer (read-string "Enter buffer name: " "*scratch*"))))

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

(global-set-key (kbd "<f5>") 'rangi-refresh-buffer)


;; auto refresh buffer
(use-package autorevert
  :delight auto-revert-mode
  :config
  (global-auto-revert-mode 1)

  ;; don't refresh non-file buffer
  (setq global-auto-revert-non-file-buffers t)

  ;; don't print anything after refresh
  (setq auto-revert-verbose nil))


;; switch between buffer
(global-set-key (kbd "C-<tab>") 'mode-line-other-buffer)


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

  ;; set filter group by vc on active
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
  (bind-key "C-c m s" 'rangi-switch-ibuffer-filter-groups ibuffer-mode-map))



(use-package buffer-move
  :config
  (setq buffer-move-stay-after-swap t))


(provide 'init-buffer)
