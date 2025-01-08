;;
;; eshell
;; ----------------------------------------------------------------------------
;;

;; set eshell directory
(setq rangi-eshell-directory (expand-file-name "eshell" rangi-emacs-cache-directory))
(unless (file-exists-p rangi-eshell-directory)
  (make-directory rangi-eshell-directory))

(setq eshell-directory-name rangi-eshell-directory)


;;
;; term
;; ----------------------------------------------------------------------------
;;

;; easy access shell
(use-package shell-pop
  :init
  (setq shell-pop-full-span t)
  (setq shell-pop-universal-key "s-t")
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-autocd-to-working-dir nil))


;; term
(setq term-buffer-maximum-size 262144)
(setq term-suppress-hard-newline t)
(setq term-scroll-to-bottom-on-output t)
(bind-key "C-c C-y" 'term-paste)


(provide 'init-shell)
