;;; init-project.el --- Project management related configuration -*- lexical-binding: t; no-byte-compile: t -*-
(use-package project
  :bind-keymap ("s-p" . project-prefix-map)
  :bind (
         :map project-prefix-map
         ("C" . project-recompile)
         ("v" . rangi-project-vc-dir))
  :config
  ;; use ibuffer when list project buffers
  (setq project-buffers-viewer 'project-list-buffers-ibuffer)
  ;; commands to choose after switching to a project
  (setq project-switch-commands '((project-find-file "Find file")
                                  (project-find-regexp "Find regexp")
                                  (project-find-dir "Find directory")
                                  (rangi-project-vc-dir "Magit/VC-Dir")
                                  (project-any-command "Other")))
  ;; store projects data in cache directory
  (setq project-list-file (expand-file-name "projects" rangi-emacs-cache-directory)))

(defun rangi-project-vc-dir ()
  "open magit or vc-dir"
  (interactive)
  (let ((p (project-current)))
    (cond ((and p (string-equal "Git" (nth 1 p)))
           (magit-status (nth 2 p)))
          (p
           (vc-dir (project-root p)))
          (t (message "Currently not in a project")))))


(provide 'init-project)
