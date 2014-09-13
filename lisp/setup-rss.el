(require 'use-package)
(require 's)
(require 'f)
(require 'dash)

(use-package elfeed
  :init
  (progn
    (setq-default elfeed-db-directory "/ramsey/Dropbox/rss/")
    (setq-default elfeed-feeds
                  (-filter (lambda (s) (not (s-starts-with? "#" s)))
                           (s-split "\n" (f-read "/ramsey/Dropbox/rss/feeds.txt") t))))
  :bind (("C-c <f9>" . elfeed)))

(provide 'setup-rss)
