;;; functions.el --- utilities functions which are required by setup files

(defun rl/sub-gen-dir (sub-dir-name)
  "Create a directory under `rl/gen-dir' and return full dir path of it"
  (if (boundp 'rl/gen-dir)
    (let ((sub-dir-path (expand-file-name sub-dir-name rl/gen-dir)))
      (make-directory sub-dir-path t)
      (file-name-as-directory sub-dir-path))
    (error "`rl/gen-dir' is not defined")))

(provide 'functions)
