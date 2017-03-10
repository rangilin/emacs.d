;; Use use-package to install and lazy loading packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)


(provide 'module-package)
