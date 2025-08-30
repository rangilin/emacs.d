;;; init-func.el --- reusable functions

(require 'thingatpt)

(defun rangi-text-at-point ()
  "Get string at point without properties, nil if no word is found"
  (let ((word (word-at-point)))
    (when word (substring-no-properties word))))


(defun rangi-default-value ()
  "Get default value, first it looks up text in region, then text at point. If not return empty string instead"
  (cond
   ((region-active-p) (buffer-substring-no-properties (mark) (point)))
   ((let ((word (rangi-text-at-point)))
      (when (> (length word) 0) word)))
   (t "")))


(defun rangi-prompt (prompt)
  "Prompt with message `prompt' and with default value as initial content"
  (interactive)
  (read-from-minibuffer prompt (rangi-default-value)))


(defun rangi-browse-url (prompt url-format)
  "Browse URL with prompt as format arguments"
  (interactive)
  (let* ((result (rangi-prompt prompt))
         (url (format url-format result)))
    (unless (zerop (length result))
      (browse-url url))))


(defun rangi-open-url (prompt url-format)
  "Open URL with prompt as format arguments"
  (interactive)
  (let* ((result (rangi-prompt prompt))
         (url (format url-format result)))
    (unless (zerop (length result))
      (shell-command (format "open %s" url)))))


(provide 'init-func)
