;;; functions.el --- utilities functions which are required by setup files

(defun rangi/sub-gen-dir (sub-dir-name)
  "Create a directory under `rangi/gen-dir' and return full dir path of it"
  (if (boundp 'rangi/gen-dir)
    (let ((sub-dir-path (expand-file-name sub-dir-name rangi/gen-dir)))
      (make-directory sub-dir-path t)
      (file-name-as-directory sub-dir-path))
    (error "`rangi/gen-dir' is not defined")))

(provide 'functions)
