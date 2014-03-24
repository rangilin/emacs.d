(require 'use-package)

(defun rangi/browse-fy (word)
  (interactive "s Let me FY that for you: ")
  (unless (zerop (length word))
    (browse-url (format "http://cdict.net/?q=%s" word))))

(defun rangi/browse-google (word)
  (interactive "s Let me Google that for you: ")
  (unless (zerop (length word))
    (browse-url (format "https://www.google.com.tw/#q=%s" word))))

(bind-key "C-S-b f" 'rangi/browse-fy)
(bind-key "C-S-b g" 'rangi/browse-google)

(provide 'setup-browser)
