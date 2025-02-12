;;; init-browse.el --- Browse stuff from Emacs  -*- lexical-binding: t -*-

(require 'thingatpt)

;; some existing functions
(bind-key "C-c b o" 'browse-url-at-point)
(bind-key "C-c b <mouse-1>" 'browse-url-at-mouse)


;; browse search engine
;; ---------------------------------------------------------------------
(defun rangi-browse-search-engine ()
  (interactive)
  (rangi--browse-url-with-prompt
   "Search in DuckDuckGo: "
   "https://duckduckgo.com/?q=%s"))

(bind-key "C-c b s" 'rangi-browse-search-engine)


;; browse file with external app
;; ---------------------------------------------------------------------
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

(bind-key "C-c b e" 'rangi-browse-in-external-app)
(with-eval-after-load "dired"
  (define-key dired-mode-map "E" 'rangi-browse-in-external-app))


;; browse dictionary
;; ---------------------------------------------------------------------
(defun rangi-browse-dictionary ()
  (interactive)
  (let ((result (rangi--prompt "Search in macOS dictionary: ")))
    (unless (zerop (length result))
      (shell-command (format "open dict://%s" result))
      (message "Send %s to dictionary" result))))

(bind-key "C-c b d" 'rangi-browse-dictionary)



(defun rangi--browse-url-with-prompt (prompt url-format)
  "Browse URL with prompt as format arguments"
  (interactive)
  (let* ((result (rangi--prompt prompt))
         (url (format url-format result)))
    (unless (zerop (length result))
      (browse-url url)
      (message "Send %s to browser" url))))

(defun rangi--prompt (prompt)
  "Prompt with message `prompt' and with default value as initial content"
  (interactive)
  (read-from-minibuffer prompt (rangi--default-value)))

(defun rangi--default-value ()
  "Get default value, first it looks up text in region, then text at point. If not return empty string instead"
  (cond
   ((region-active-p) (buffer-substring-no-properties (mark) (point)))
   ((let ((word (rangi--text-at-point)))
      (when (> (length word) 0) word)))
   (t "")))

(defun rangi--text-at-point ()
  "Get string at point without properties, nil if no word is found"
  (let ((word (word-at-point)))
    (when word (substring-no-properties word))))


(provide 'init-browse)
