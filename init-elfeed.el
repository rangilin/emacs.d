(require-package 'elfeed)
(require 'elfeed)
(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :before "2 weeks ago"
                              :remove 'unread))

(provide 'init-elfeed)
