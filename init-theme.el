;;;; Theme

(require-package 'color-theme-sanityinc-solarized)
(require-package 'color-theme-sanityinc-tomorrow)

(setq-default custom-enabled-themes '(sanityinc-tomorrow-eighties))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)

(provide 'init-theme)
