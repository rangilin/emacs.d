(require 'thingatpt)

(defun rangi-search ()
  "Search on the search engine"
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


;; modify based on http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html
(defun rangi-browse-in-external-app (&optional filename)
  "Open the current file or dired marked files in external app"
  (interactive)
  (let* ((file-list
          (if filename
              (progn (list filename))
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
         (do-it-p (if (<= (length file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when do-it-p
      (mapc
       (lambda (fpath)
         (shell-command
          (concat "open " (shell-quote-argument fpath))))  file-list))))


(defun rangi-shell-command-with-prompt (prompt url-format)
  "Browse URL with prompt as format arguments"
  (interactive)
  (let* ((result (rangi-prompt prompt))
         (url (format url-format result)))
    (unless (zerop (length result))
      (browse-url url)
      (message "Send %s to browser" url))))


(defun rangi-browse-dictionary ()
  (interactive)
  (let ((result (rangi-prompt "Search in macOS dictionary: ")))
    (unless (zerop (length result))
      (shell-command (format "open dict://%s" result))
      (message "Send %s to dictionary" result))))


(global-set-key (kbd "C-c b s") 'rangi-search)
(global-set-key (kbd "C-c b p") 'browse-url-at-point)
(global-set-key (kbd "C-c b <mouse-1>") 'browse-url-at-mouse)
(global-set-key (kbd "C-c b o") 'rangi-browse-in-external-app)
(with-eval-after-load "dired"
  (define-key dired-mode-map "E" 'rangi-browse-in-external-app))
(global-set-key (kbd "C-c b d") 'rangi-browse-dictionary)


(provide 'init-browse)
