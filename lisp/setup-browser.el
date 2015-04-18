(require 'functions)
(require 'use-package)

(defun rangi/browse-fy ()
  "Dictionary look up with prompt"
  (interactive)
  (rangi/browse-url-with-prompt "Let me FY that for you: "
                                "http://cdict.net/?q=%s"))

(defun rangi/browse-google ()
  "Google with prompt"
  (interactive)
  (rangi/browse-url-with-prompt "Let me Google that for you: "
                                "https://www.google.com.tw/#q=%s"))

(defun rangi/browse-url-with-prompt (prompt url-format)
  "Browse URL with prompt as format arguments"
  (interactive)
  (let* ((result (rangi-prompt prompt))
         (url (format url-format result)))
    (unless (zerop (length result))
      (browse-url url)
      (message "Send %s to browser" url))))

(bind-key "C-c B f" 'rangi/browse-fy)
(bind-key "C-c B g" 'rangi/browse-google)

(provide 'setup-browser)
