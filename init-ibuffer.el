;; Most stuff borrowed from
;; https://github.com/purcell/emacs.d/blob/master/init-ibuffer.el
(require-package 'ibuffer-vc)

(defun ibuffer-set-up-preferred-filters ()
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'filename/process)
    (ibuffer-do-sort-by-filename/process)))

(add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters)

;;----------------------------------------------------------------------------
;; Use human readable size column instead of original one
;;----------------------------------------------------------------------------
(eval-after-load 'ibuffer
  '(progn
     (define-ibuffer-column size-h
       (:name "Size" :inline t)
       (cond
        ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
        ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
        (t (format "%8d" (buffer-size)))))))

;;----------------------------------------------------------------------------
;; Explicitly require ibuffer-vc to get its column definitions, which can't
;; be autoloaded
;;----------------------------------------------------------------------------
(eval-after-load 'ibuffer
  '(require 'ibuffer-vc))

;;----------------------------------------------------------------------------
;; Modify the default ibuffer-formats
;;----------------------------------------------------------------------------
(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              filename-and-process)))



(setq ibuffer-filter-group-name-face 'font-lock-doc-face)

(provide 'init-ibuffer)
