;;; zig-ts-mode.el --- Major mode for Zig code -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 meowking <mr.meowking@tutamail.com>

;; Author: meowking <mr.meowking@tutamail.com>
;; Homepage: https://codeberg.org/meow_king/zig-ts-mode
;; Keywords: zig languages tree-sitter

;; Package-Version: 0.3.0
;; Package-Requires: ((emacs "29.1"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provide font-lock, indentation, and navigation for the Zig
;; programming language <https://ziglang.org/>.
;;
;; For the tree-sitter grammar this mode is based on, see
;; <https://github.com/tree-sitter-grammars/tree-sitter-zig/>.

;;; Code:

(require 'treesit)
(require 'seq)
(require 'rx)

(defgroup zig-ts nil
  "Major mode for editing Zig code."
  :link '(url-link "https://codeberg.org/meow_king/zig-ts-mode")
  :link '(url-link "https://ziglang.org")
  :prefix "zig-ts-"
  :group 'languages)

(defcustom zig-ts-indent-offset 4
  "Number of spaces for each indentation step in `zig-ts-mode'."
  :type 'natnum
  :safe 'natnump
  :group 'zig-ts)

(defcustom zig-ts-zig-bin "zig"
  "Path to zig executable."
  :type 'file
  :safe #'stringp)

(defcustom zig-ts-run-optimization-mode "Debug"
  "Optimization mode to run code with."
  :type '(choice (const :tag "Optimizations off and safety on" "Debug")
                 (const :tag "Optimizations on and safety on" "ReleaseSafe")
                 (const :tag "Optimizations on and safety off" "ReleaseFast")
                 (const :tag "Size optimizations on and safety off" "ReleaseSmall"))
  :safe #'stringp)

(defcustom zig-ts-test-optimization-mode "Debug"
  "Optimization mode to run tests with."
  :type '(choice (const :tag "Optimizations off and safety on" "Debug")
                 (const :tag "Optimizations on and safety on" "ReleaseSafe")
                 (const :tag "Optimizations on and safety off" "ReleaseFast")
                 (const :tag "Size optimizations on and safety off" "ReleaseSmall"))
  :safe #'stringp)

(defvar zig-ts--syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments: Zig only has // comments.
    ;;
    ;; 12 means: start of a 2-char comment sequence (1st and 2nd char
    ;; are the same)
    (modify-syntax-entry ?/   ". 12" table)
    (modify-syntax-entry ?\n  ">"    table)

    ;; Strings and Chars
    (modify-syntax-entry ?\"  "\""   table)
    (modify-syntax-entry ?\'  "\""   table)
    (modify-syntax-entry ?\\  "\\"   table) ;; Escape character

    ;; Symbol constituents (variable names)
    (modify-syntax-entry ?_   "_"    table)

    ;; Punctuation / Operators
    ;; These are mostly "." by default, but being explicit is good practice
    (modify-syntax-entry ?+   "."    table)
    (modify-syntax-entry ?-   "."    table)
    (modify-syntax-entry ?=   "."    table)
    (modify-syntax-entry ?%   "."    table)
    (modify-syntax-entry ?&   "."    table)
    (modify-syntax-entry ?|   "."    table)
    (modify-syntax-entry ?^   "."    table)
    (modify-syntax-entry ?!   "."    table)
    (modify-syntax-entry ?@   "."    table)
    (modify-syntax-entry ?~   "."    table)
    (modify-syntax-entry ?<   "."    table)
    (modify-syntax-entry ?>   "."    table)
    (modify-syntax-entry ?*   "."    table) ; Distinct from Rust (no block comments)
    (modify-syntax-entry ?.   "."    table)

    table)
  "Syntax table for `zig-ts-mode'.")

;;;; Font-locking
;;
;;
;; See https://github.com/tree-sitter-grammars/tree-sitter-zig/blob/master/queries/highlights.scm
;;
;; Ideally the font-locking should be aligned with the upstream highlights.scm.

(defconst zig-ts--keywords
  '("asm" "defer" "errdefer" "test" "error" "const" "var"
    "struct" "union" "enum" "opaque"
    "async" "await" "suspend" "nosuspend" "resume"
    "fn"
    "and" "or" "orelse"
    "return"
    "if" "else" "switch"
    "for" "while" "break" "continue"
    "usingnamespace" "export"
    "try" "catch"
    "volatile" "allowzero" "noalias" "addrspace" "align" "callconv" "linksection" "pub"
    "inline" "noinline" "extern" "comptime" "packed" "threadlocal")
  "Zig keywords for tree-sitter font-locking.")

(defconst zig-ts--operators
  '("=" "*=" "*%=" "*|=" "/=" "%=" "+=" "+%=" "+|=" "-=" "-%=" "-|=" "<<=" "<<|=" ">>="
    "&=" "^=" "|=" "!" "~" "-" "-%" "&" "==" "!=" ">" ">=" "<=" "<" "^" "&" "|" "<<" ">>"
    "<<|" "+" "++" "+%" "-%" "+|" "-|" "*" "/" "%" "**" "*%" "*|" "||" ".*" ".?" "?" "..")
  "Zig operators for tree-sitter font-locking.")

(defvar zig-ts--font-lock-feature-list
  '((comment definition)
    (keyword string type)
    (assignment builtin constant escape-sequence label number)
    (bracket delimiter error function operator property variable))
  "`treesit-font-lock-feature-list' for `zig-ts-mode'.")

(defvar zig-ts--font-lock-settings
  (treesit-font-lock-rules
   :language 'zig
   :feature 'bracket
   '((["[" "]" "(" ")" "{" "}"]) @font-lock-bracket-face
     (payload "|" @font-lock-bracket-face))

   :language 'zig
   :feature 'builtin
   '(([(builtin_identifier) "c" "..."]) @font-lock-builtin-face
     (calling_convention "(" _ @font-lock-builtin-face ")")
     (((identifier) @font-lock-builtin-face)
      (:equal "_" @font-lock-builtin-face)))

   :language 'zig
   :feature 'comment
   '((((comment) @font-lock-doc-face)
      (:match "^//!" @font-lock-doc-face))
     (comment) @font-lock-comment-face)

   :language 'zig
   :feature 'delimiter
   '(([";" "." "," ":" "=>" "->"]) @font-lock-delimiter-face)

   :language 'zig
   :feature 'definition
   '((function_declaration name: (identifier) @font-lock-function-name-face)
     (struct_declaration (container_field
                          name: (identifier) @font-lock-property-name-face))
     (enum_declaration (container_field
                        name: (identifier) @font-lock-property-name-face))
     (union_declaration (container_field
                         name: (identifier) @font-lock-property-name-face))
     (variable_declaration (identifier) @font-lock-type-face
                           "=" [(struct_declaration)
                                (enum_declaration)
                                (union_declaration)
                                (opaque_declaration)])
     (variable_declaration [("var" (identifier) @font-lock-variable-name-face)
                            ("const" (identifier) @font-lock-constant-face)]
                           "=" (identifier) @font-lock-variable-use-face)
     (variable_declaration
      :anchor [("const" (identifier) @font-lock-constant-face)
               ("var" (identifier) @font-lock-variable-name-face)]))

   :language 'zig
   :feature 'assignment
   '((initializer_list
      (assignment_expression
       left: (field_expression
              "."
              member: (identifier) @font-lock-property-name-face)))
     (variable_declaration (field_expression
                            "."
                            member: (identifier) @font-lock-property-use-face)))

   :language 'zig
   :feature 'function
   '((call_expression function: (identifier) @font-lock-function-call-face)
     (call_expression
      function: (field_expression
                 member: (identifier) @font-lock-function-call-face)))

   :language 'zig
   :feature 'keyword
   `(([,@zig-ts--keywords]) @font-lock-keyword-face)

   :language 'zig
   :feature 'number
   '(([(integer) (float)]) @font-lock-number-face)

   :language 'zig
   :feature 'operator
   `(([,@zig-ts--operators]) @font-lock-operator-face)

   :language 'zig
   :feature 'string
   '(([(character) (string) (multiline_string)]) @font-lock-string-face)

   :language 'zig
   :feature 'type
   '((parameter type: (identifier) @font-lock-type-face)
     (enum_declaration (container_field
                        type: (identifier) @font-lock-type-face))
     ((identifier) @font-lock-type-face
      (:match "^[A-Z_][a-zA-Z0-9_]*" @font-lock-type-face))
     (([(builtin_type) "anyframe"]) @font-lock-type-face))

   :language 'zig
   :feature 'label
   '((block_label (identifier) @font-lock-constant-face)
     (break_label (identifier) @font-lock-constant-face))

   :language 'zig
   :feature 'property
   '((field_expression "." member: (identifier) @font-lock-property-use-face)
     (call_expression
      function: (field_expression
                 object: (field_expression
                          member: (identifier) @font-lock-property-use-face))))

   ;; Must be under type, otherwise can be highlighted as constants
   :language 'zig
   :feature 'constant
   '(([(boolean) "null" "unreachable" "undefined"]) @font-lock-constant-face
     ((identifier) @font-lock-constant-face
      (:match "^[A-Z][A-Z_0-9]+$" @font-lock-constant-face)))

   :language 'zig
   :feature 'variable
   '((field_initializer "." (identifier) @font-lock-variable-use-face)
     (identifier) @font-lock-variable-use-face)

   :language 'zig
   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :language 'zig
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "`treesit-font-lock-settings' for `zig-ts-mode'.")

;;;; Indentation
;;
;; Tree-sitter indentation rules for Zig
;;
;; Adapted from nvim indentation queries in nvim-treesitter

(defvar zig-ts--indent-rules
  `((zig
     ;; Top-level definitions: column 0
     ((parent-is "source_file") column-0 0)

     ;; Closing delimiters align with the opening construct
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") parent-bol 0)

     ((parent-is "multiline_string") prev-line 0)

     ((node-is ";") parent-bol 0)

     ((match "else" "if") parent-bol 0)
     ((parent-is "if") parent-bol zig-ts-indent-offset)
     ((parent-is "else") parent-bol zig-ts-indent-offset)

     ((parent-is "while") parent-bol zig-ts-indent-offset)

     ((parent-is "for") parent-bol zig-ts-indent-offset)

     ((parent-is "switch") parent-bol zig-ts-indent-offset)

     ((parent-is "assignment_expression") parent-bol 0)
     ((parent-is "field_expression") parent-bol zig-ts-indent-offset)

     ((parent-is "variable_declaration") parent-bol zig-ts-indent-offset)
     ((parent-is "struct_declaration") parent-bol zig-ts-indent-offset)
     ((parent-is "enum_declaration") parent-bol zig-ts-indent-offset)
     ((parent-is "union_declaration") parent-bol zig-ts-indent-offset)
     ((parent-is "container_field") parent-bol zig-ts-indent-offset)
     ((parent-is "initializer_list") parent-bol zig-ts-indent-offset)

     ((parent-is "block") parent-bol zig-ts-indent-offset)
     ((parent-is "arguments") parent-bol zig-ts-indent-offset)

     (no-node parent-bol 0)))
  "`treesit-simple-indent-rules' for `zig-ts-mode'.")

;;;; Find the definition at point (some Emacs commands use this internally)

(defvar zig-ts--defun-type-regexp
  (regexp-opt '("declaration"))
  "Regex matching tree-sitter node types treated as defun-like.
Used as the value of `treesit-defun-type-regexp'.")

(defun zig-ts--imenu-fn-pred-fn (node)
  "Test whether the given function NODE is validated.
See `treesit-simple-imenu-settings'."
  (if (equal (treesit-node-type node) "function_declaration")
      t
    ;; VarDecl
    ;; assume camelCase is a function
    (let ((case-fold-search nil))
      (string-match-p
       "^[a-z]+\\([A-Z][a-z0-9]*\\)+$"
       (treesit-node-text
        (treesit-node-child-by-field-name node "variable_type_function"))))))

(defun zig-ts--imenu-func-name-fn (node)
  "Return appropriate name for the given function NODE.
See `treesit-simple-imenu-settings'."
  (treesit-node-text (treesit-node-child-by-field-name node "name")))

(defun zig-ts--imenu-assign-style-declaration-name-fn (node)
  "Return appropriate name for the given enum NODE.
See `treesit-simple-imenu-settings'."
  (let ((parent-node (treesit-node-parent node)))
    (when (equal (treesit-node-type parent-node) "variable_declaration")
      (treesit-node-text (treesit-node-child parent-node 0 t)))))

(defun zig-ts--imenu-test-name-fn (node)
  "Return appropriate name for the given test NODE.
See `treesit-simple-imenu-settings'."
  (treesit-node-text (treesit-node-child node 0 t)))

(defun zig-ts--imenu-type-pred-fn (node)
  "Test whether the given type NODE is validated.
See `treesit-simple-imenu-settings'."
  ;; assume TitleCase is a type
  (let ((case-fold-search nil))
    (string-match-p
     "^[A-Z]\\([a-z]+[A-Za-z_0-9]*\\)*$"
     (treesit-node-text
      (treesit-node-child-by-field-name node "variable_type_function")))))

(defun zig-ts--imenu-type-name-fn (node)
  "Return appropriate name for the given type NODE.
See `treesit-simple-imenu-settings'."
  (treesit-node-text
   (treesit-node-child-by-field-name node "variable_type_function")))

(defun zig-ts--imenu-constant-pred-fn (node)
  "Test whether the given constant NODE is validated.
See `treesit-simple-imenu-settings'."
  ;; assume TitleCase is a type
  (let ((case-fold-search nil))
    (and
     (equal
      (treesit-node-text (treesit-node-child node 0))
      "const")
     ;; assume CAPS_1 is a constant
     (string-match-p
      "^[A-Z][A-Z_0-9]+$"
      (treesit-node-text
       (treesit-node-child-by-field-name node "variable_type_function"))))))

(defun zig-ts--imenu-constant-name-fn (node)
  "Return appropriate name for the given constant NODE.
See `treesit-simple-imenu-settings'."
  (treesit-node-text
   (treesit-node-child-by-field-name node "variable_type_function")))

(defun zig-ts--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ("function_declaration" (zig-ts--imenu-func-name-fn node))
    (_ (zig-ts--imenu-assign-style-declaration-name-fn node))))

;;;; Imenu integration

(defvar zig-ts--imenu-settings
  `(("Fn" "\\`function_declaration\\'" nil zig-ts--imenu-func-name-fn)
    ("Enum" "\\`enum_declaration\\'"
     nil zig-ts--imenu-assign-style-declaration-name-fn)
    ("Struct" "\\`struct_declaration\\'"
     nil zig-ts--imenu-assign-style-declaration-name-fn)
    ("Opaque" "\\`opaque_declaration\\'"
     nil zig-ts--imenu-assign-style-declaration-name-fn)
    ("ErrorSet" "\\`error_set_declaration\\'"
     nil zig-ts--imenu-assign-style-declaration-name-fn)
    ("Union" "\\`union_declaration\\'"
     nil zig-ts--imenu-assign-style-declaration-name-fn)
    ("Test" "\\`test_declaration\\'" nil zig-ts--imenu-test-name-fn))
  "Settings for `treesit-simple-imenu' in `zig-ts-mode'.")

;;;; Structured navigation

(defvar zig-ts--thing-settings
  `((zig
     (defun ,(regexp-opt '("function_declaration")))
     (sentence ,(regexp-opt '("variable_declaration"
                              "defer_statement"
                              "expression_statement")))
     (text ,(regexp-opt '("comment" "string" "multiline_string")))
     (comment "comment")))
  "Settings for `treesit-thing-settings' in `zig-ts-mode'.")

;;;; Fill paragraph

(defun zig-ts--fill-paragraph (&optional _justify)
  "Fill the Zig paragraph at point.
Use tree-sitter to detect multiline-string and doc-comment.  Return t if
point is in a multiline_string block or doc comment block, otherwise let
the default handler run."
  (let* ((node (treesit-node-at (point)))
         (type (treesit-node-type node))
         (doc-comment-p (and (string= type "comment")
                             (save-excursion
                               (goto-char (treesit-node-start node))
                               (looking-at "//!"))))
         (multiline-string-p (string= type "multiline_string")))
    (when (or doc-comment-p multiline-string-p)
      ;; Return t so `fill-paragraph' doesn't attempt to fill by itself
      t)))

;;;; Comment continuation (M-j)

(defun zig-ts--comment-indent-new-line (&optional soft)
  "Break line at point and indent, continuing comment if within one.
SOFT works the same as in `comment-indent-new-line'."
  (let ((insert-line-break (lambda ()
                             (delete-horizontal-space)
                             (if soft
                                 (insert-and-inherit ?\n)
                               (newline 1)))))
    (cond
     ;; Line starts with //, or ///, or ////..., or //!
     ((save-excursion
        (beginning-of-line)
        (re-search-forward (rx "//" (group (* (any "/!")) (* " ")))
                           (line-end-position) t nil))
      (let ((offset (- (match-beginning 0) (line-beginning-position)))
            (whitespaces (match-string 1)))
        (funcall insert-line-break)
        (delete-region (line-beginning-position) (point))
        (insert (make-string offset ?\s) "//" whitespaces)))

     ;; Line starts with multiline string
     ((save-excursion
        (beginning-of-line)
        (re-search-forward (rx "\\\\" (group (* " ")))
                           (line-end-position) t nil))
      (let ((offset (- (match-beginning 0) (line-beginning-position)))
            (whitespaces (match-string 1)))
        (funcall insert-line-break)
        (delete-region (line-beginning-position) (point))
        (insert (make-string offset ?\s) "\\\\" whitespaces)))

     ;; Line starts with whitespaces or no space.  This is basically
     ;; the default case since (rx (* " ")) matches anything
     ((save-excursion
        (beginning-of-line)
        (looking-at (rx (* " "))))
      (let ((whitespaces (match-string 0)))
        (funcall insert-line-break)
        (delete-region (line-beginning-position) (point))
        (insert whitespaces))))))

;;;; Pre-check

;; TODO: add markdown-inline support
(defconst zig-ts-grammar-recipes
  '((zig . ("https://github.com/tree-sitter-grammars/tree-sitter-zig")))
  "Tree-sitter grammar recipes for Zig.
Each entry is a list of (LANG . (URL REVISION SOURCE-DIR)).  Suitable
for use as the value of `treesit-language-source-alist'.")

(defun zig-ts-install-grammars (&optional force)
  "Install required language grammars if not already available.

With prefix argument FORCE, reinstall grammars even if they are already
installed.  This is useful after upgrading `zig-ts-mode' to a version that
requires a newer grammar."
  (interactive "P")
  (dolist (recipe zig-ts-grammar-recipes)
    (let ((grammar (car recipe)))
      (when (or force (not (treesit-language-available-p grammar)))
        (message "Installing %s tree-sitter grammar..." grammar)
        ;; Make `treesit-install-language-grammar' pick up the grammar
        ;; recipes without modifying user's configuration
        (let ((treesit-language-source-alist zig-ts-grammar-recipes))
          (treesit-install-language-grammar grammar))))))

(defvar zig-ts--grammar-compatibility-checked nil
  "Non-nil if grammar compatibility has already been checked this session.")

(defun zig-ts--check-grammar-compatibility ()
  "Check whether installed grammars are compatible with current `zig-ts-mode'.
Emit a warning if an outdated grammar is detected."
  (unless zig-ts--grammar-compatibility-checked
    (setq zig-ts--grammar-compatibility-checked t)
    (when (treesit-language-available-p 'zig)
      (condition-case nil
          ;; old tree-sitter use ValDecl
          (treesit-query-compile 'zig '((variable_declaration)) t)
        (error
         (display-warning
          'zig
          (format "The installed `%s' grammar does not appear to be from \
tree-sitter-grammars.  Run C-u M-x zig-ts-install-grammars to reinstall."
                  'zig)))))))

;;;; CLI commands
;;
;; copied from zig-mode

(defun zig-ts--run-cmd (cmd &optional source &rest args)
  "Use compile command to execute a zig CMD with ARGS if given.
If given a SOURCE, execute the CMD on it."
  (let ((cmd-args (if source (cons source args) args)))
    (save-some-buffers)
    (compilation-start (mapconcat 'shell-quote-argument
                                  `(,zig-ts-zig-bin ,cmd ,@cmd-args)
                                  " "))))

(defun zig-ts-build ()
  "Compile using `zig build`."
  (interactive)
  (zig-ts--run-cmd "build"))

(defun zig-ts-build-exe ()
  "Create executable from source or object file."
  (interactive)
  (zig-ts--run-cmd "build-exe" (file-local-name (buffer-file-name))))

(defun zig-ts-build-lib ()
  "Create library from source or assembly."
  (interactive)
  (zig-ts--run-cmd "build-lib" (file-local-name (buffer-file-name))))

(defun zig-ts-build-obj ()
  "Create object from source or assembly."
  (interactive)
  (zig-ts--run-cmd "build-obj" (file-local-name (buffer-file-name))))

(defun zig-ts-test ()
  "Test buffer using `zig test`."
  (interactive)
  (zig-ts--run-cmd "test" (file-local-name (buffer-file-name)) "-O" zig-ts-test-optimization-mode))

(defun zig-ts-run ()
  "Create an executable from the current buffer and run it immediately."
  (interactive)
  (zig-ts--run-cmd "run" (file-local-name (buffer-file-name)) "-O" zig-ts-run-optimization-mode))

;;;; Major mode definitions

(defvar-keymap zig-ts-mode-map
  :doc "Keymap for `zig-ts-mode'."
  "C-c C-b C-b" #'zig-ts-build
  "C-c C-b C-e" #'zig-ts-build-exe
  "C-c C-b C-l" #'zig-ts-build-lib
  "C-c C-b C-o" #'zig-ts-build-obj
  "C-c C-r" #'zig-ts-run
  "C-c C-t" #'zig-ts-test)

;;;###autoload
(define-derived-mode zig-ts-mode prog-mode "Zig"
  "Major mode for editing Zig, powered by tree-sitter."
  :group 'zig-ts
  :syntax-table zig-ts--syntax-table

  (when-let* ((missing (seq-filter (lambda (r)
                                     (not (treesit-language-available-p (car r))))
                                   zig-ts-grammar-recipes)))
    (when (y-or-n-p "Zig tree-sitter grammars are not installed yet.  Install now?")
      (zig-ts-install-grammars)))

  ;; Warn if installed grammars are outdated
  (zig-ts--check-grammar-compatibility)

  ;; Compile
  (setq-local compile-command "zig build")

  ;; Comment
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip (rx (group (seq "/" (+ "/")))
                                     (* (syntax whitespace))))
  (setq-local comment-end-skip (rx (* (syntax whitespace))
                                   (group (syntax comment-end))))
  (setq-local comment-line-break-function #'zig-ts--comment-indent-new-line)

  ;; Electric indentation on delimiters
  (setq-local electric-indent-chars
              (append "{}()[].,;" electric-indent-chars))

  ;; Fill paragraph
  (setq-local paragraph-start
              (rx (or (seq (* (syntax whitespace))
                           (group (* (seq "/" (+ "/"))))
                           (* (syntax whitespace))
                           eol)
                      "\f")))
  (setq-local paragraph-separate paragraph-start)
  (setq-local adaptive-fill-mode t)

  (when (treesit-ready-p 'zig)
    (treesit-parser-create 'zig)

    ;; Fill paragraph with tree-sitter feature
    (setq-local fill-paragraph-function #'zig-ts--fill-paragraph)

    ;; Font-lock
    (setq-local treesit-font-lock-settings zig-ts--font-lock-settings)
    (setq-local treesit-font-lock-feature-list zig-ts--font-lock-feature-list)

    ;; Indentation
    (setq-local treesit-simple-indent-rules zig-ts--indent-rules)

    ;; Navigation
    (setq-local treesit-simple-imenu-settings zig-ts--imenu-settings)
    (setq-local treesit-thing-settings zig-ts--thing-settings)

    ;; Defun
    (setq-local treesit-defun-name-function #'zig-ts--defun-name)
    (setq-local treesit-defun-type-regexp zig-ts--defun-type-regexp)

    (treesit-major-mode-setup)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(zig\\|zon\\)\\'" . zig-ts-mode))

(provide 'zig-ts-mode)
;;; zig-ts-mode.el ends here
