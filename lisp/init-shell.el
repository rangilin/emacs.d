;;;; eshell

;; set eshell directory
(defconst rangi-eshell-directory (expand-file-name "eshell" rangi-generated-files-directory))

(unless (file-exists-p rangi-eshell-directory)
  (make-directory rangi-eshell-directory))

(setq eshell-directory-name rangi-eshell-directory)





(provide 'init-shell)
