(use-package project
  :bind-keymap (("s-p" . project-prefix-map)
                ("C-c p" . project-prefix-map))
  :bind (:map project-prefix-map
              ("s" . rangi-project-ag)
              ("4 f" . rangi-project-find-file-other-window)
              ("v" . rangi-project-vc-dir))
  :config
  (setq project-list-file (expand-file-name "projects" rangi-emacs-cache-directory)))


(defun rangi-project-ag ()
  "use ag search current files in project"
  (interactive)
  (let ((p (project-current)))
    (if p
        (counsel-ag nil (project-root p))
      (message "Currently not in a project"))))

(defun rangi-project-vc-dir ()
  "open magit or vc-dir"
  (interactive)
  (let ((p (project-current)))
    (cond ((and p (string-equal "Git" (nth 1 p)))
           (magit-status))
          (p
           (vc-dir (project-root p)))
          (t (message "Currently not in a project")))))

(defun rangi-project-find-file-other-window ()
  "find project file but open it in other window"
  (interactive)
  (split-window-right)
  (other-window 1)
  (project-find-file))


(provide 'init-project)
