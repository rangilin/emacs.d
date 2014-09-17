(require 'use-package)

(use-package org
  :mode ("\\.org$" . org-mode)
  :init
  (progn
    (setq-default org-directory "/ramsey/Dropbox/org")
    (setq-default org-special-ctrl-a/e t)
    (setq-default org-startup-truncated nil)
    (setq-default org-startup-with-inline-images t)
    (setq-default org-drawers `("PROPERTIES" "CLOCK" "LOGBOOK" "RESULTS" "DETAIL"))

    (add-hook 'org-shiftup-final-hook 'windmove-up)
    (add-hook 'org-shiftleft-final-hook 'windmove-left)
    (add-hook 'org-shiftdown-final-hook 'windmove-down)
    (add-hook 'org-shiftright-final-hook 'windmove-right)

    (defadvice org-beginning-of-line
      (before advice-org-beginning-of-line activate)
      (handle-shift-selection))
    (defadvice org-end-of-line
      (before advice-org-end-of-line activate)
      (handle-shift-selection))
    (defadvice org-table-beginning-of-field
      (before advice-org-table-beginning-of-field activate)
      (handle-shift-selection))
    (defadvice org-table-end-of-field
      (before advice-org-table-end-of-field activate)
      (handle-shift-selection))

    (use-package gnuplot)
    (use-package org-plot
      :bind (("C-M-g" . org-plot/gnuplot)))))

(provide 'setup-org)
