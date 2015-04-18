(defun rangi-show-init-time ()
  "Show how much time init.el took to load"
  (message "init.el took %.2f seconds to load" (float-time (time-subtract after-init-time before-init-time))))

(provide 'setup-benchmark)
