;;
;; eshell
;; ----------------------------------------------------------------------------
;;

;; set eshell directory
(setq rangi-eshell-directory (expand-file-name "eshell" rangi-generated-files-directory))
(unless (file-exists-p rangi-eshell-directory)
  (make-directory rangi-eshell-directory))

(setq eshell-directory-name rangi-eshell-directory)


;;
;; term
;; ----------------------------------------------------------------------------
;;

;; easy access shell
(use-package shell-pop
  :config
  (custom-set-variables
   '(shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
   '(shell-pop-universal-key "s-=")))


;; term
(setq term-buffer-maximum-size 262144)
(setq term-suppress-hard-newline t)
(setq term-scroll-to-bottom-on-output t)


(provide 'init-shell)
