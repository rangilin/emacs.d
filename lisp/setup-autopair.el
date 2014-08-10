(require 'use-package)

(use-package autopair
  :diminish autopair-mode
  :init
  (progn
    (autopair-global-mode)

    (defun rangi/autopair-always-pair ()
      (set (make-local-variable 'autopair-pair-criteria) 'always)
      (set (make-local-variable 'autopair-skip-criteria) 'always))
    ;; make autopair always insert pair, so *) syntax in org-mode won't screw up autopair
    (add-hook 'org-mode-hook 'rangi/autopair-always-pair)
    (add-hook 'sql-mode-hook 'rangi/autopair-always-pair)

    (defun rangi/autopair-turn-off ()
      (autopair-mode -1))
    (add-hook 'term-mode-hook 'rangi/autopair-turn-off)))

(provide 'setup-autopair)
