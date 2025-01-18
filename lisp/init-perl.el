(defun rangi-cperl-mode-map ()
  (interactive)
  (set-transient-map
   (let ((rangi-cperl-mode-map (make-sparse-keymap)))
     (define-key rangi-cperl-mode-map "d" 'cperl-perldoc-at-point)
     (define-key rangi-cperl-mode-map "D" 'cperl-perldoc)
     (which-key-show-keymap 'rangi-cperl-mode-map t)
     rangi-cperl-mode-map)))

(use-package cperl-mode
  :mode "\\.pl\\'"
  :interpreter "perl"
  :bind (:map cperl-mode-map
              ("s-m" . rangi-cperl-mode-map)
              ("C-c m" . rangi-cperl-mode-map))
  :config
  (setq cperl-invalid-face nil)
  :hook ((cperl-mode . eglot-ensure)))

(provide 'init-perl)
