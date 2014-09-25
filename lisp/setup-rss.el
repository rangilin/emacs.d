(require 'use-package)
(require 's)
(require 'f)
(require 'dash)
(require 'variables)

(use-package elfeed
  :init
  (progn
    (setq-default elfeed-db-directory rangi/elfeed-directory)
    (setq-default elfeed-feeds
                  (-filter (lambda (s) (not (s-starts-with? "#" s)))
                           (s-split "\n" (f-read rangi/elfeed-feeds-file) t))))
  :bind (("C-c <f9>" . elfeed)))

(provide 'setup-rss)
