;;; functions.el --- utilities functions which are required by setup files
(require 'thingatpt)

;; -- OS specified -------------------------------------------------------------
;; http://doc.rix.si/org/fsem.html#sec-2-1

(defun rangi-gnulinuxp ()
  "Returns t if the system is a GNU/Linux machine, otherwise nil"
  (string-equal system-type "gnu/linux"))

(defun rangi-osxp ()
  "Returns t if the system is a Mac OS X machine, otherwise nil"
  (string-equal system-type "darwin"))

(defun rangi/sub-gen-dir (sub-dir-name)
  "Create a directory under `rangi/gen-dir' and return full dir path of it"
  (if (boundp 'rangi/gen-dir)
    (let ((sub-dir-path (expand-file-name sub-dir-name rangi/gen-dir)))
      (make-directory sub-dir-path t)
      (file-name-as-directory sub-dir-path))
    (error "`rangi/gen-dir' is not defined")))

(defun rangi/prompt (prompt)
  "Prompt with message `prompt' and with default value as initial content"
  (interactive)
  (read-from-minibuffer prompt (rangi/default-value)))

(defun rangi/default-value ()
  "Get default value, first it looks up text in region, then text at point. If not return empty string instead"
  (cond
   ((region-active-p) (buffer-substring-no-properties (mark) (point)))
   ((let ((word (rangi/text-at-point)))
      (when (> (length word) 0) word)))
   (t "")))

(defun rangi/text-at-point ()
  "Get string at point without properties, nil if no word is found"
  (let ((word (word-at-point)))
    (when word (substring-no-properties word))))

(defun rangi/minor-mode-on-p (mode)
  "Check whether minor mode `mode' is on, `mode' should be a symbol specifying
the minor mode"
  (and (boundp mode) (symbol-value mode)))

(defun rangi-open-startup-file ()
  (find-file rangi/startup-file))

(provide 'functions)
