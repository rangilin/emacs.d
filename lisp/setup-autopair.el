(require 'use-package)

(use-package autopair
  :init
  (progn
    (autopair-global-mode)

    ;; make autopair always insert pair, so *) syntax in org-mode won't screw up autopair
    (add-hook 'org-mode-hook
              #'(lambda ()
                  (set (make-local-variable 'autopair-pair-criteria) 'always)
                  (set (make-local-variable 'autopair-skip-criteria) 'always)))))

(provide 'setup-autopair)
