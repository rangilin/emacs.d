;;; init-misc.el --- Misc initialization file -*- lexical-binding: t -*-
(bind-key "C-c E r" 'restart-emacs)

;; package to edit beancount file
(use-package beancount
  :load-path "site-lisp/beancount-mode"
  :mode ("\\.beancount\\'" . beancount-mode))


;; enable repeat mode
(use-package repeat
  :config
  (setq set-mark-command-repeat-pop t)
  (repeat-mode))


;; docker
(use-package dockerfile-mode
  :ensure t
  :pin nongnu
  :mode "Dockerfile\\'")


;; yaml
(use-package yaml-mode
  :ensure t
  :pin nongnu
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)
         ("\\.ansible-lint\\'" . yaml-mode)))


;; MacOS
(setq mac-command-modifier 'super)
(setq mac-right-command-modifier 'super)
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'meta)
(setq-default locate-command "mdfind")


;; URL
(defun rangi-browse-duckduckgo ()
  (interactive)
  (rangi-browse-url "Search DuckDuckGo: " "https://duckduckgo.com/?q=%s"))

(defun rangi-open-dictionary ()
  (interactive)
  (rangi-open-url "Search Dictionary: " "dict://%s"))

(bind-key "C-c o o" 'browse-url-at-point)
(bind-key "C-c o <mouse-1>" 'browse-url-at-mouse)
(bind-key "C-c o s" 'rangi-browse-duckduckgo)
(bind-key "C-c o d" 'rangi-open-dictionary)


;; csv mode
(use-package csv-mode
  :pin gnu
  :ensure t
  :mode "\\.csv\\'"
  :config
  (setq csv-separators '("," ";" "|" " " "\t")))


;; eshell
(setq rangi-eshell-directory (expand-file-name "eshell" rangi-emacs-cache-directory))
(unless (file-exists-p rangi-eshell-directory)
  (make-directory rangi-eshell-directory))
(setq-default eshell-directory-name rangi-eshell-directory)


;; tramp
(use-package tramp
  :defer t
  :config
  ;; for debugging tramp
  ;; (setq-default tramp-debug-buffer t)
  ;; (setq-default tramp-verbose 10)

  ;; assign tramp file location
  (setq tramp-persistency-file-name (expand-file-name "tramp" rangi-emacs-cache-directory))

  ;; clean all tramp connections & buffers
  (defun rangi-tramp-cleanup-all ()
    (interactive)
    (tramp-cleanup-all-buffers)
    (tramp-cleanup-all-connections)
    (message "Cleaned up all tramp buffers & connections"))
  (bind-key "C-c t c" 'rangi-tramp-cleanup-all))


;; bookmark
(setq bookmark-default-file (expand-file-name "bookmark" rangi-emacs-cache-directory))


;; mise
(use-package mise
  :load-path "site-lisp/mise"
  :diminish
  :hook (emacs-startup . global-mise-mode)
  :init
  (use-package inheritenv
    :load-path "site-lisp/inheritenv"))


;; LLM client
(use-package gptel
  :pin nongnu
  :ensure t
  :bind (("C-c a n" . rangi-gptel-new-session)
         ("C-c a s" . gptel-send)
         ("C-c a S" . gptel-menu))

  :init
  (setq rangi-gptel-directive-dev-plan "
You are a senior software engineer acting as a collaborative partner working with me.
I am solo dev that runs a one man software company, your goals is to help me clarifying my problems regarding software development and provide advices and answers best to your knowledge.

Rules:
- Be concise, direct and to the point.
- Be thorough, but keep it grounded in practical, real-world execution.
- Ask questions to clarify problems if missing important detail.
")

  ;; Set this before gptel is loaded so default directive can be pickup by it.
  (setq-default gptel-directives
                `((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
                  (dev-plan . ,(string-trim rangi-gptel-directive-dev-plan))))

  (defun rangi-gptel-new-session ()
    "Create a new, dedicated gtpel buffer with my own naming format"
    (interactive)
    (let* ((prefix (concat "llm-session-" (format-time-string "%Y%m%d%H%M") "-"))
           (name (read-string "Creating new LLM session: " prefix)))
      (switch-to-buffer (gptel name))))

  :config
  ;; set default model
  (setq gptel-model 'deepseek/deepseek-v4-flash)
  ;; configure backend, using OpenRouter
  (setq gptel-backend
        (gptel-make-openai "OpenRouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key (lambda () (string-trim (shell-command-to-string "op read 'op://Personal/OpenRouter/API Keys/default'")))
          :models '(deepseek/deepseek-v4-flash openai/gpt-5.4-nano openai/gpt-5.6-luna))))


(provide 'init-misc)
