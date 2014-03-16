(require 'use-package)

(use-package org
  :mode ("\\.org$" . org-mode)
  :init
  (progn
    (use-package gnuplot)
    (use-package org-plot
      :bind (("C-M-g" . org-plot/gnuplot))))
  :config
  (progn
    (setq-default org-directory "/ramsey/Dropbox/org")
    (setq-default org-special-ctrl-a/e t)
    (setq-default org-startup-truncated nil)
    (setq-default org-startup-with-inline-images t)
    (setq-default org-drawers `("PROPERTIES" "CLOCK" "LOGBOOK" "RESULTS" "DETAIL"))

    ;; workaround windmove conflict
    (add-hook 'org-shiftup-final-hook 'windmove-up)
    (add-hook 'org-shiftleft-final-hook 'windmove-left)
    (add-hook 'org-shiftdown-final-hook 'windmove-down)
    (add-hook 'org-shiftright-final-hook 'windmove-right)))

(provide 'setup-org)
