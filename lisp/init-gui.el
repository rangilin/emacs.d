;; maximize frame
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; stop cursor jumping around while scrolling
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)

;; improve speed when cursor jumping
(setq auto-window-vscroll nil)

;; turn off these UIs
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; disable column & line number for performance
(column-number-mode -1)
(line-number-mode -1)
;; use this when I need to know line number
(bind-key "s-=" 'what-line)

;; allow cursor to act on visual line by default
(visual-line-mode)

;; display path of current buffer in the frame title
(setq-default frame-title-format '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b"))))

;; no alarm bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; don't blink cursor
(blink-cursor-mode -1)

;; set up fringe
(fringe-mode '(nil . 10))
(setq-default indicate-buffer-boundaries 'right)
(setq-default indicate-empty-lines t)

;; resize frame by pixel
(setq frame-resize-pixelwise t)

;; adjust scrolling speed of mouse/trackpad
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))


;;
;; Windows
;;----------------------------------------------------------------------------
;;

;; split window vertically if window width is more than 120, otherwise do it horizontally
(setq split-height-threshold nil)
(setq split-width-threshold 120)

;; change window with simpler keystroke
(global-set-key (kbd "C-1") (kbd "C-x 1"))
(global-set-key (kbd "C-2") (kbd "C-x 2"))
(global-set-key (kbd "C-3") (kbd "C-x 3"))
(global-set-key (kbd "C-0") (kbd "C-x 0"))



;; restore window configurations easiler
(use-package winner
  :bind (("M-s-[" . winner-undo)
         ("M-s-]" . winner-redo)
         ;; workaround for accented characters
         ("M-s-“" . winner-undo)
         ("M-s-‘" . winner-redo))
  :config
  (winner-mode 1))


(use-package windmove
  :config
  (global-set-key (kbd "<C-s-left>")  'windmove-left)
  (global-set-key (kbd "s-[")  'windmove-left)
  (global-set-key (kbd "<C-s-right>") 'windmove-right)
  (global-set-key (kbd "s-]")  'windmove-right)
  (global-set-key (kbd "<C-s-up>")    'windmove-up)
  (global-set-key (kbd "s-}")  'windmove-up)
  (global-set-key (kbd "<C-s-down>")  'windmove-down)
  (global-set-key (kbd "s-{")  'windmove-down))


(use-package transpose-frame)



;;
;; Search Completion
;;----------------------------------------------------------------------------
;;

;; use ivy to do do completion everywhere
(use-package ivy
  :delight
  :bind
  (("s-b" . ivy-switch-buffer)
   ("C-c C-r" . ivy-resume))
  :config
  (ivy-mode 1)
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d "))

;; install ivy-hydra for additional hydra keybindings
(use-package ivy-hydra)

;; use swiper to search local file
(use-package swiper)

;; use ivy for emacs commands
(use-package counsel
  :bind
  (("C-s" . counsel-grep-or-swiper)
   ("s-f" . counsel-grep-or-swiper)
   ("M-x" . counsel-M-x)
   ("s-P" . counsel-M-x)
   ("C-x C-f" . rangi-counsel-find-file)
   ("s-o" . rangi-counsel-find-file)
   ("C-x C-r" . counsel-recentf)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-find-library)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char))
  :config

  (setq counsel-grep-swiper-limit 10000000)

  (defun rangi-counsel-find-file (arg)
    (interactive "P")
    (if arg (counsel-file-jump)
      (counsel-find-file))))


(use-package ivy-rich
  :config
  (ivy-rich-mode 1)
  ;; use abbrevation file path
  (setq ivy-rich-path-style 'abbrev))



;;
;; Fonts
;;----------------------------------------------------------------------------
;;

;; allow emacs to display most unicode emoji properly
(set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'prepend)

;; make font used by variable-pitch face to make it looks better
(set-face-font 'variable-pitch "Noto Sans CJK TC")

;;
;; Match the size of English and CJK fonts so they align.
;;
;; Based on: https://github.com/coldnew/coldnew-emacs
;;
;;
;; A quick test to show whether characters are aligned:
;;
;; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa (40 chars)
;; 測測測測測測測測測測測測測測測測測測測測 (20 chars)
;; あいうえおあいうえおあいうえおあいうえお (20 chars)
;;

;; predefined font sets

(defvar rangi-font-alist-roboto-and-noto-sans
  '((english-font . "Roboto Mono")
    (cjk-font . "Noto Sans Mono CJK TC")
    (default-size-pair . (14 . 16))
    (size-pairs . ((10 . 12) (12 . 14) (14 . 16) (15 . 18) (16 . 20) (18 . 22) (20 . 24)
                   (22 . 26) (24 . 28) (26 . 32) (28 . 34) (30 . 36) (34 . 40) (36 . 44)))))

(defvar rangi-font-alist-sometype-and-noto-sans
  '((english-font . "Sometype Mono")
    (cjk-font . "Noto Sans Mono CJK TC")
    (default-size-pair . (16 . 18))
    (size-pairs . ((10 . 12) (12 . 14) (14 . 16) (16 . 18) (18 . 20) (20 . 24) (22 . 26)
                   (24 . 28) (26 . 30) (28 . 32) (30 . 34) (32 . 38) (34 . 40)))))

(defvar rangi-font-alist-monaco-and-noto-sans
  '((english-font . "Monaco")
    (cjk-font . "Noto Sans Mono CJK TC")
    (default-size-pair . (14 . 16))
    (size-pairs . ((10 . 12) (12 . 14) (14 . 16) (16 . 20) (18 . 22) (20 . 24) (22 . 26)))))


(defvar rangi-font-alist rangi-font-alist-monaco-and-noto-sans "Current font set")
(defvar rangi-font-size-pair (cdr (assoc 'default-size-pair rangi-font-alist)) "Current font size pair")

(defun rangi-font-exist-p (fontname)
  "Test if this font is exist or not. This function only work on GUI mode,
on terminal it just return nil since you can't set font for emacs on it."
  (if (or (not fontname) (string= fontname "") (not (display-graphic-p)))
      nil
    (if (not (x-list-fonts fontname))
        nil t)))

(defun rangi-set-font-size (size-pair)
  "Set size of current font set"
  (message "font size set to (%d . %d)" (car size-pair) (cdr size-pair))
  (let ((english (cdr (assoc 'english-font rangi-font-alist)))
        (cjk (cdr (assoc 'cjk-font rangi-font-alist))))

    (if (rangi-font-exist-p english)
        (set-frame-font (format "%s:pixelsize=%d" english (car size-pair)) t t)
      (message "font %s does not exist" english))

    (if (rangi-font-exist-p cjk)
        (dolist (charset '(kana han hangul cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font) charset
                            (font-spec :family cjk :size (cdr size-pair))))
      (message "font %s does not exist" cjk))))


(defun rangi-step-font-size (step)
  "Increase/Decrease emacs's font size."
  (let ((scale-steps (assoc 'size-pairs rangi-font-alist)))
    (if (< step 0) (setq scale-steps (reverse scale-steps)))
    (setq rangi-font-size-pair
          (or (cadr (member rangi-font-size-pair scale-steps))
              rangi-font-size-pair))
    (when rangi-font-size-pair
      (rangi-set-font-size rangi-font-size-pair))))


(defun rangi-increase-emacs-font-size ()
  "Decrease emacs's font size acording font set."
  (interactive)
  (rangi-step-font-size 1))

(defun rangi-decrease-emacs-font-size ()
  "Increase emacs's font size acording font set."
  (interactive)
  (rangi-step-font-size -1))

(defun rangi-reset-emacs-font-size ()
  "Reset emacs's font size to default size of current font set."
  (interactive)
  (let ((pair (cdr (assoc 'default-size-pair rangi-font-alist))))
    (setq rangi-font-size-pair pair)
    (rangi-set-font-size pair)))

(global-set-key (kbd "C-=") 'rangi-increase-emacs-font-size)
(global-set-key (kbd "C--") 'rangi-decrease-emacs-font-size)
(global-set-key (kbd "C-\\") 'rangi-reset-emacs-font-size)

(rangi-set-font-size rangi-font-size-pair)



;;
;; Themes & Appearances
;;----------------------------------------------------------------------------
;;

(use-package solarized-theme
  :config

  (defun rangi-light-theme ()
    (interactive)
    (load-theme 'solarized-light t)
    (rangi--common-theme))

  (defun rangi-dark-theme ()
    (interactive)
    (load-theme 'solarized-dark t)
    (rangi--common-theme))

  (defun rangi--common-theme()
    (let ((fg (face-attribute 'default :foreground))
          (bg (face-attribute 'default :background)))

      ;; increase mode line height
      (set-face-attribute 'mode-line nil :box `(:line-width 5 :color ,(face-attribute 'mode-line :background)))
      (set-face-attribute 'mode-line-inactive nil :box `(:line-width 5 :color ,(face-attribute 'mode-line-inactive :background)))

      ;; put underline below the font bottom line so mode line looks better
      (setq x-underline-at-descent-line t)))

  (defun rangi--disable-all-themes ()
    "disable all themes."
    (dolist (i custom-enabled-themes)
      (disable-theme i)))

  (bind-key "C-c e t l" 'rangi-light-theme)
  (bind-key "C-c e t d" 'rangi-dark-theme)

  ;;
  ;; auto switch theme at sunrise/sunset
  ;;
  (use-package solar
    :ensure nil
    :config

    (setq calendar-location-name "Taipei, Taiwan")
    (setq calendar-latitude 25.04)
    (setq calendar-longitude 121.51)

    (defun rangi-sunrise ()
      (let* ((sunrise (caar (solar-sunrise-sunset (calendar-current-date))))
             (sunrise-hour (floor sunrise))
             (sunrise-minute (floor (* 60 (mod sunrise 1)))))
        (message "%s %s %s" sunrise sunrise-hour sunrise-minute)))

    (defun rangi-sunset ()
      (let* ((sunset (caadr (solar-sunrise-sunset (calendar-current-date))))
             (sunset-hour (floor sunset))
             (sunset-minute (floor (* 60 (mod sunset 1)))))
        (message "%s %s %s" sunset sunset-hour sunset-minute))))


  ;; TODO: using (current-time) format to calculate
  (decode-time (time-to-seconds))
  (time-to-seconds (encode-time 0 5 0 31 1 2000))
  (format-time-string "%F %T" (time-to-seconds (encode-time 0 5 0 31 1 2000)))



  (rangi-light-theme))


;; highlight numbers in prog-mode
(use-package highlight-numbers
  :defer t
  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

;; highlight keywords
(use-package hl-todo
  :init
  (add-hook 'prog-mode-hook #'hl-todo-mode)
  (add-hook 'markdown-mode-hook #'hl-todo-mode))

;; make title bar match with emacs theme, cool !
(use-package ns-auto-titlebar
  :config
  (ns-auto-titlebar-mode))



;;
;; Treemacs
;;----------------------------------------------------------------------------
;;


(use-package treemacs
  :config
  (setq treemacs-persist-file (expand-file-name "treemacs-persist" user-emacs-directory))
  (setq treemacs-last-error-persist-file (expand-file-name "treemacs-persist-at-last-error" rangi-generated-files-directory))
  (setq treemacs-no-png-images t)
  (setq treemacs-is-never-other-window t)
  (setq treemacs-recenter-after-file-follow t)
  (setq treemacs-no-delete-other-windows t)

  (defun rangi-treemacs-toggle ()
    (interactive)
    (pcase (treemacs-current-visibility)
      ('visible
       (if (treemacs-is-treemacs-window-selected?)
           (delete-window (treemacs-get-local-window))
         (treemacs-select-window)))
      ('exists (treemacs-select-window))
      ('none (treemacs))))

  (defun rangi-treemacs-deselect ()
    (interactive)
    (when (treemacs-is-treemacs-window-selected?)
      (other-window 1)))

  (defun rangi-treemacs-close ()
    (interactive)
    (when (eq (treemacs-current-visibility) 'visible)
      (delete-window (treemacs-get-local-window))))

  (defun rangi-treemacs-project-root ()
    (treemacs-copy-project-root)
    (pop kill-ring))

  (defun rangi-treemacs-jump-to-file ()
    (interactive)
    (treemacs-select-window)
    (let ((root (rangi-treemacs-project-root)))
      (rangi-treemacs-deselect)
      (counsel-file-jump nil root)))

  (defun rangi-treemacs-jump-to-dired ()
    (interactive)
    (treemacs-select-window)
    (let ((root (rangi-treemacs-project-root)))
      (rangi-treemacs-deselect)
      (counsel-dired-jump nil root)))



  ;; jump actions
  (bind-key "j f" 'rangi-treemacs-jump-to-file treemacs-mode-map)
  (bind-key "C-c j f" 'rangi-treemacs-jump-to-file)
  (bind-key "j d" 'rangi-treemacs-jump-to-dired treemacs-mode-map)
  (bind-key "C-c j d" 'rangi-treemacs-jump-to-dired)

  ;; workspace actions
  (bind-key "W s" 'treemacs-switch-workspace treemacs-mode-map)
  (bind-key "W d" 'treemacs-remove-workspace treemacs-mode-map)
  (bind-key "W e" 'treemacs-edit-workspaces treemacs-mode-map)
  (bind-key "W c" 'treemacs-create-workspace treemacs-mode-map)

  ;; window actions
  (bind-key "s-1" 'rangi-treemacs-toggle)
  (bind-key "<S-escape>" 'rangi-treemacs-close)
  (bind-key "<escape>" 'rangi-treemacs-deselect treemacs-mode-map)

  ;; node traversal actions
  (bind-key "<left>" 'treemacs-collapse-parent-node treemacs-mode-map)
  (bind-key "<right>" 'treemacs-TAB-action treemacs-mode-map))





(defhydra hydra-fwb-arrangement (:color amaranth :hint none)
  "
  Arranging window & buffer & frame ... (_q_: Quit)
  ___: Shrink window horizontally     _j_: Move window down    _J_: Move buffer down    _d_: Rotate frame clockwise ↻
  _+_: Enlarge window horizontally    _k_: Move window up      _K_: Move buffer up      _a_: Rotate frame anticlockwise ↺
  _-_: Shrink window                  _l_: Move window right   _L_: Move buffer right   _w_: Flip frame ⇕
  _=_: Enlarge window                 _h_: Move window left    _H_: Move buffer left    _s_: Flop frame ⟺
  ^ ^                                 ^ ^                      ^ ^                      _e_: Transpose frame /
  "

  ("_" shrink-window-horizontally)
  ("+" enlarge-window-horizontally)
  ("-" shrink-window)
  ("=" enlarge-window)

  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("h" windmove-left)

  ("J" buf-move-down)
  ("K" buf-move-up)
  ("L" buf-move-right)
  ("H" buf-move-left)

  ("d" rotate-frame-clockwise)
  ("a" rotate-frame-anticlockwise)
  ("w" flip-frame)
  ("s" flop-frame)
  ("e" transpose-frame)

  ("q" nil "Quit"))

(global-set-key (kbd "C-c a") 'hydra-fwb-arrangement/body)


(provide 'init-gui)
