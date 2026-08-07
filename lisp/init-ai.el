;;; init-ai.el --- AI related configurations -*- lexical-binding: t -*-


;;;;;;;;;;;;;
;; Prompts ;;
;;;;;;;;;;;;;

;; Me
(setq rangi-ai-prompt-me "
I am a solo founder who runs a one man software company.
")


;; Roles
(setq rangi-ai-prompt-role-default "
You are a large language model living in Emacs and a helpful assistant.
")

(setq rangi-ai-prompt-role-dev "
You are a senior software engineer acting as a collaborative partner working with me. Your goals is to help me understand technical problems and advises me best to your knowledge. You are thorough, practical, and understand that a good engineering is about making trade-offs, not perfect decisions, and your answers reflect these qualities.
")


;; Rules
(setq rangi-ai-prompt-rule-common "
Unless instructed otherwise, you follow these rules when answering my requests:
- Be concise, direct and to the point, do not be conversational.
- Ask questions to clarify problems if missing important detail.
- Noted in the response if unsure of answers.
")


;; Scenarios
(setq rangi-ai-prompt-scenario-session "
You are currently in a conversation session with me working toward a specific goal.

How a session works:
- There will be a session goal, it is the overarching theme of the conversation.
- There will be session context, it contains information regarding the goal and surrounding elements.
- Previous conversions will be included if they exist.
- My latest request will be included at the end of the text.
- Understand the goal, context, and previous conversions, then answer my lastest request.
- Do not lead the conversation.
")

;; Directives
(setq rangi-ai-directive-default
      (string-trim
       (concat rangi-ai-prompt-me
               rangi-ai-prompt-role-default
               rangi-ai-prompt-rule-common)))

(setq rangi-ai-directive-dev
      (string-trim
       (concat rangi-ai-prompt-me
               rangi-ai-prompt-role-dev
               rangi-ai-prompt-rule-common)))

(setq rangi-ai-directive-dev-in-session
      (string-trim
       (concat rangi-ai-prompt-me
               rangi-ai-prompt-role-dev
               rangi-ai-prompt-rule-common
               rangi-ai-prompt-scenario-session)))


;;;;;;;;;;;
;; gptel ;;
;;;;;;;;;;;

(use-package gptel
  :pin nongnu
  :ensure t
  :bind (("C-c a n" . rangi-gptel-new-session)
         ("C-c a s" . gptel-send)
         ("C-c a S" . gptel-menu))

  :init
  ;; Set this before gptel is loaded so default directive can be picked up by it.
  (setq-default gptel-directives
                `((default . ,rangi-ai-directive-default)
                  (dev . ,rangi-ai-directive-dev)
                  (dev-session . ,rangi-ai-directive-dev-in-session)))

  (defun rangi-gptel-new-session ()
    "Create a new, dedicated gtpel buffer with my own naming format"
    (interactive)
    (let* ((prefix (concat "llm-session-" (format-time-string "%Y%m%d%H%M") "-"))
           (name (read-string "Creating new LLM session: " prefix)))
      (switch-to-buffer (gptel name))))

  :config
  ;; set default mode used for gptel buffer
  (setq gptel-default-mode 'org-mode)

  ;; change prefix
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "** ~user:\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "** ~agent:\n")

  ;; include files that linked in the prompt
  (setq gptel-track-media t)

  ;; set default model
  (setq gptel-model '~deepseek/deepseek-v4-flash-latest)

  ;; configure backend, using OpenRouter
  (setq gptel-backend
        (gptel-make-openai "OpenRouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key (lambda () (string-trim (shell-command-to-string "op read 'op://Personal/OpenRouter/API Keys/default'")))
          :models '(~deepseek/deepseek-v4-flash-latest openai/gpt-5.4-nano openai/gpt-5.6-luna))))


(provide 'init-ai)
