(require 'thingatpt)

(defun rangi-search ()
  "Search"
  (interactive)
  (rangi-browse-url-with-prompt
   "Search in DuckDuckGo: "
   "https://duckduckgo.com/?q=%s"))

(defun rangi-browse-url-with-prompt (prompt url-format)
  "Browse URL with prompt as format arguments"
  (interactive)
  (let* ((result (rangi-prompt prompt))
         (url (format url-format result)))
    (unless (zerop (length result))
      (browse-url url)
      (message "Send %s to browser" url))))

(defun rangi-prompt (prompt)
  "Prompt with message `prompt' and with default value as initial content"
  (interactive)
  (read-from-minibuffer prompt (rangi-default-value)))

(defun rangi-default-value ()
  "Get default value, first it looks up text in region, then text at point. If not return empty string instead"
  (cond
   ((region-active-p) (buffer-substring-no-properties (mark) (point)))
   ((let ((word (rangi-text-at-point)))
      (when (> (length word) 0) word)))
   (t "")))

(defun rangi-text-at-point ()
  "Get string at point without properties, nil if no word is found"
  (let ((word (word-at-point)))
    (when word (substring-no-properties word))))


(global-set-key (kbd "C-c B s") 'rangi-search)
(global-set-key (kbd "C-c B p") 'browse-url-at-point)
(global-set-key (kbd "C-c B <mouse-1>") 'browse-url-at-mouse)


(provide 'init-browse)
