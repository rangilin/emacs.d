(require 'thingatpt)

(defun rl-search ()
  "Search"
  (interactive)
  (rl-browse-url-with-prompt
   "Let me search that for you: "
   "https://duckduckgo.com/?q=%s"))

(defun rl-browse-url-with-prompt (prompt url-format)
  "Browse URL with prompt as format arguments"
  (interactive)
  (let* ((result (rl-prompt prompt))
         (url (format url-format result)))
    (unless (zerop (length result))
      (browse-url url)
      (message "Send %s to browser" url))))

(defun rl-prompt (prompt)
  "Prompt with message `prompt' and with default value as initial content"
  (interactive)
  (read-from-minibuffer prompt (rl-default-value)))

(defun rl-default-value ()
  "Get default value, first it looks up text in region, then text at point. If not return empty string instead"
  (cond
   ((region-active-p) (buffer-substring-no-properties (mark) (point)))
   ((let ((word (rl-text-at-point)))
      (when (> (length word) 0) word)))
   (t "")))

(defun rl-text-at-point ()
  "Get string at point without properties, nil if no word is found"
  (let ((word (word-at-point)))
    (when word (substring-no-properties word))))


(bind-key "C-c B s" 'rl-search)


(provide 'module-browse)
