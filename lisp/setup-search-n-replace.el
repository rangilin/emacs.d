(require 'use-package)

;; ------------------------------ anzu
(use-package anzu
  :diminish ""
  :init (global-anzu-mode +1)
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp)))

;; ------------------------------ isearch
(setq-default isearch-allow-scroll 1)

(eval-after-load "isearch"
  '(progn
     (defadvice
       isearch-repeat-forward
       (after isearch-repeat-forward-recenter activate)
       (recenter))
     (defadvice
       isearch-repeat-backward
       (after isearch-repeat-backward-recenter activate)
       (recenter))))

;; ------------------------------ color moccur
(use-package color-moccur
  :config
  (progn
    (defun moccur-view-file ()
      "my version that will recenter when select an item"
      (if (string= moccur-before-buffer-name moccur-buffer-name)
          (moccur-color-check-view)
        (if moccur-current-line-overlays
            (progn
              (delete-overlay moccur-current-line-overlays)
              (setq moccur-overlays nil)))
        (moccur-color-view))

      (switch-to-buffer-other-window
       (get-buffer moccur-buffer-name))
      (goto-line (string-to-number moccur-line))
      (if (re-search-forward moccur-regexp-color (line-end-position) t)
          ()
        (goto-line (string-to-number moccur-line)))

      ;; color
      (moccur-color-current-line)
      (recenter)

      (setq moccur-before-buffer-name moccur-buffer-name)
      (switch-to-buffer-other-window moccur-mocur-buffer)))
  :bind (("C-o" . occur-by-moccur) ("C-S-o" . moccur)))

;; ------------------------------ dabbrev highlights
(use-package dabbrev-highlight
  :load-path "site-lisp/dabbrev-highlight")

(provide 'setup-search-n-replace)
