;;; opencode-tools.el --- OpenCode tools for gptel -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains tool implementations using gptel-make-tool.
;; It includes both enhanced versions of existing tools from llm.el
;; and new tools ported from opencode.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-util)
(require 'pp)
(require 'seq)
(require 'subr-x)
(require 'gptel)
(require 'opencode-descriptions)
(require 'opencode-treesit)

;; Permission system variables
(defcustom opencode-bash-permissions '(("*" . ask))
  "Permission settings for bash commands.
Each entry is (PATTERN . ACTION) where PATTERN is a shell glob and ACTION is one of
the symbols `allow`, `deny`, or `ask'. Later entries override earlier ones."
  :type '(repeat (cons (string :tag "Glob pattern")
                       (choice :tag "Action" (const allow) (const deny) (const ask))))
  :group 'opencode)

(defcustom opencode-edit-permissions 'ask
  "Permission setting for file edits and file creation.
Set to one of the symbols `allow`, `deny`, or `ask'."
  :type '(choice (const allow) (const deny) (const ask))
  :group 'opencode)

;; Helper functions
(defun opencode--get-tool-prop (tool prop)
  "Get property PROP from TOOL (plist or gptel-tool struct).
TOOL can be either a plist or a gptel-tool struct.
PROP should be a keyword like :name or :category."
  (if (listp tool)
      (plist-get tool prop)
    (let* ((prop-name (substring (symbol-name prop) 1))
           (accessor (intern-soft (format "gptel-tool-%s" prop-name))))
      (if (and accessor (fboundp accessor))
          (funcall accessor tool)
        nil))))

(defun opencode--normalize-permission (value)
  "Normalize VALUE from the permissions alist to a symbol."
  (cond
   ((memq value '(allow deny ask)) value)
   ((and (stringp value) (not (string-empty-p value)))
    (pcase (intern (downcase value))
      ('allow 'allow)
      ('deny 'deny)
      ('ask 'ask)
      (_ 'ask)))
   (t 'ask)))

(defun opencode--match-pattern (pattern command)
  "Return non-nil when COMMAND matches shell glob PATTERN."
  (condition-case nil
      (string-match-p (wildcard-to-regexp pattern) command)
    (error nil)))

(defun opencode--permission-action (command permissions)
  "Return the action for COMMAND given PERMISSIONS."
  (let ((action 'ask))
    (dolist (entry permissions action)
      (let ((pattern (car entry))
            (value (opencode--normalize-permission (cdr entry))))
        (when (and (stringp pattern)
                   (opencode--match-pattern pattern command))
          (setq action value))))))

(defun opencode--check-permission (command permissions)
  "Check if COMMAND is allowed based on PERMISSIONS.
Returns t when the action is permitted, nil otherwise. Prompts the user when
the action resolves to `ask'."
  (pcase (opencode--permission-action command permissions)
    ('allow t)
    ('deny nil)
    ('ask (y-or-n-p (format "Execute command: %s? " command)))
    (_ t)))

(defun opencode--edit-permission-allowed-p ()
  "Return non-nil when edits are permitted under `opencode-edit-permissions'."
  (pcase (opencode--normalize-permission opencode-edit-permissions)
    ('allow t)
    ('deny nil)
    ('ask (y-or-n-p "Apply file modifications? "))
    (_ t)))

(defun opencode--truthy (value)
  "Return non-nil when VALUE represents a truthy JSON boolean."
  (not (memq value '(nil :json-false))))

(defun opencode--format-diagnostics (diagnostics)
  "Turn DIAGNOSTICS (a list of plists) into a printable string."
  (when (and diagnostics (listp diagnostics))
    (let ((lines
           (cl-loop for diag in diagnostics
                    when (and (plist-get diag :line)
                              (plist-get diag :end-line)
                              (plist-get diag :message))
                    collect
                    (format "Line %d:%d-%d:%d | %s | %s"
                            (plist-get diag :line)
                            (plist-get diag :column)
                            (plist-get diag :end-line)
                            (plist-get diag :end-column)
                            (upcase (symbol-name (or (plist-get diag :severity) 'info)))
                            (plist-get diag :message))))))
      (when lines
        (string-join lines "\n"))))

(defun opencode--append-diagnostics (base-output diagnostics)
  "Append DIAGNOSTICS string to BASE-OUTPUT when present."
  (if (and diagnostics (not (string-empty-p diagnostics)))
      (format "%s\n\n--- Tree-sitter Analysis ---\n%s" base-output diagnostics)
    base-output))

(defun opencode--todo->string (todos)
  "Pretty-print TODOS for tool responses."
  (with-temp-buffer
    (pp todos (current-buffer))
    (string-trim-right (buffer-string))))

(defun opencode--ensure-directory (path)
  "Ensure PATH is an existing directory."
  (let ((expanded (expand-file-name path)))
    (unless (file-directory-p expanded)
      (error "Directory does not exist: %s" expanded))
    expanded))

(defun opencode--run-shell-command (command working-dir timeout)
  "Execute COMMAND in WORKING-DIR with TIMEOUT (ms).
Returns a cons of (EXIT-CODE . OUTPUT). Signals an error on timeout."
  (let* ((timeout-ms (or timeout 120000))
         (deadline (when (> timeout-ms 0)
                     (+ (float-time) (/ timeout-ms 1000.0))))
         (default-directory (if (and working-dir (not (string-empty-p working-dir)))
                                (opencode--ensure-directory working-dir)
                              default-directory))
         (buffer (generate-new-buffer " *opencode-bash*"))
         (exit-code 0)
         (output ""))
    (unwind-protect
        (let ((process-environment process-environment)
              (process (start-file-process-shell-command
                        "opencode-bash" buffer command)))
          (while (and (process-live-p process)
                      (or (not deadline) (< (float-time) deadline)))
            (accept-process-output process 0.1))
          (when (and deadline (process-live-p process))
            (kill-process process)
            (error "Command timed out after %dms: %s" timeout-ms command))
          (setq exit-code (process-exit-status process))
          (setq output (with-current-buffer buffer
                         (buffer-string))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))
    (cons exit-code output)))


;; Enhanced tools from llm.el with opencode descriptions

(defun opencode-read-file (filepath &optional offset limit)
  "Read FILEPATH with optional OFFSET and LIMIT, returning numbered content."
  (let* ((expanded-path (expand-file-name filepath))
         (offset (max 0 (or offset 0)))
         (limit (when limit (max 0 limit))))
    (unless (file-exists-p expanded-path)
      (error "File does not exist: %s" expanded-path))
    (find-file-noselect expanded-path)
    (with-temp-buffer
      (insert-file-contents expanded-path)
      (let* ((lines (split-string (buffer-string) "\n"))
             (total (length lines))
             (end (min total (if limit (+ offset limit) total)))
             (selected
              (cl-loop for line in lines
                       for idx from 0
                       when (and (>= idx offset) (< idx end))
                       collect (cons (1+ idx) line)))
             (numbered-content
              (if selected
                  (mapconcat (lambda (entry)
                               (format "%6d\t%s" (car entry) (cdr entry)))
                             selected "\n")
                "")))
        (let* ((diag-str (opencode--format-diagnostics
                          (opencode-treesit-get-diagnostics expanded-path)))
               (content (opencode--append-diagnostics numbered-content diag-str)))
          (if (< end total)
              (format "%s\n\n(File has more lines. Use 'offset' to continue after line %d.)"
                      content end)
            content))))))

(defun opencode-run-command (command &optional working-dir timeout description)
  "Execute COMMAND in an optional WORKING-DIR with TIMEOUT and DESCRIPTION."
  (unless (and (stringp command) (not (string-empty-p command)))
    (error "Command must be a non-empty string"))
  (unless (opencode--check-permission command opencode-bash-permissions)
    (error "Command execution denied: %s" command))
  (let* ((timeout-ms (when timeout (truncate timeout)))
         (message-text (or description command)))
    (with-temp-message (format "Executing: %s" message-text)
      (pcase-let ((`(,status . ,output)
                   (opencode--run-shell-command command working-dir timeout-ms)))
        (if (= status 0)
            (if (string-empty-p output)
                "Command completed with no output."
              output)
          (error "Command failed (exit %d):\n%s" status output))))))

(defun opencode-edit-buffer (buffer-name old-string new-string)
  "Enhanced buffer editing with better error handling."
  (with-current-buffer buffer-name
    (let ((case-fold-search nil))
      (save-excursion
        (goto-char (point-min))
        (let ((count 0))
          (while (search-forward old-string nil t)
            (setq count (1+ count)))
          (cond
           ((= count 0)
            (format "Error: Could not find text to replace in buffer %s" buffer-name))
           ((> count 1)
            (format "Error: Found %d matches for the text to replace in buffer %s. Use replaceAll or provide more context." count buffer-name))
           (t
            (goto-char (point-min))
            (search-forward old-string)
            (replace-match new-string t t)
            (format "Successfully edited buffer %s" buffer-name))))))))

;; New tools from opencode

(defun opencode-glob (pattern &optional path)
  "Return files matching glob PATTERN under PATH (or current directory)."
  (unless (and (stringp pattern) (not (string-empty-p pattern)))
    (error "Glob pattern must be a non-empty string"))
  (let* ((search-path (expand-file-name (or path default-directory)))
         (limit 100)
         (results '())
         (use-rg (executable-find "rg")))
    (unless (file-directory-p search-path)
      (error "Directory does not exist: %s" search-path))
    (if use-rg
        (with-temp-buffer
          (let ((default-directory search-path)
                (args (list "--files" "--hidden" "--color=never" "--glob" pattern)))
            (pcase (apply #'process-file "rg" nil t nil args)
              (0 (setq results (split-string (buffer-string) "\n" t)))
              (1 (setq results '()))
              (code (error "Glob search failed with exit %d:\n%s"
                           code (buffer-string))))))
      (with-temp-buffer
        (pcase (apply #'process-file "find" nil t nil
                      search-path "-type" "f" "-name" pattern)
          (0 (setq results (split-string (buffer-string) "\n" t)))
          (code (error "Glob search failed with exit %d:\n%s"
                       code (buffer-string))))))
    (let* ((sorted (sort (cl-loop for file in results
                                  for expanded = (if (file-name-absolute-p file)
                                                     file
                                                   (expand-file-name file search-path))
                                  collect expanded)
                         #'string-lessp))
           (truncated (> (length sorted) limit))
           (display-list (cl-subseq sorted 0 (min limit (length sorted))))
           (body (if display-list
                     (string-join display-list "\n")
                   "No files found.")))
      (if truncated
          (format "%s\n\n(Results truncated to %d entries. Refine the pattern for more precision.)"
                  body limit)
        body))))

(defun opencode-grep (pattern &optional include path)
  "Search PATH for PATTERN using ripgrep. Optionally filter with INCLUDE glob."
  (unless (executable-find "rg")
    (error "ripgrep (rg) is required for the Grep tool"))
  (unless (and (stringp pattern) (not (string-empty-p pattern)))
    (error "Pattern must be a non-empty string"))
  (let* ((default-directory (expand-file-name (or path default-directory)))
         (args '("--color=never" "--line-number" "--no-heading" "--smart-case")))
    (unless (file-directory-p default-directory)
      (error "Directory does not exist: %s" default-directory))
    (when include
      (setq args (append args (list "--glob" include))))
    (setq args (append args (list pattern ".")))
    (with-temp-buffer
      (pcase (apply #'process-file "rg" nil t nil args)
        (0 (buffer-string))
        (1 "No matches found.")
        (code (error "ripgrep exited with %d:\n%s" code (buffer-string)))))))


(defun aider-make-repo-map (path)
  "Create or refresh a repo map in an emacs buffer using aider's util."
  (let* ((default-directory (expand-file-name path))
         (buffer-name "*Aider Repo Map*")
         (process-environment process-environment))

    ;; Set OPENROUTER_API_KEY if not already set to prevent aider complaints
    (unless (getenv "OPENROUTER_API_KEY")
      (setenv "OPENROUTER_API_KEY" "placeholder"))

    (with-temp-message (format "Generating repo map for: %s" path)
      (let ((command "uvx aider --show-repo-map --exit --no-gitignore --no-check-update --no-analytics --no-pretty")
            (output-buffer (get-buffer-create buffer-name)))

        (with-current-buffer output-buffer
          (erase-buffer)
          (let ((exit-code (call-process-shell-command command nil t nil)))
            (if (= exit-code 0)
                (progn
                  ;; Skip first 4 lines as mentioned in TODO
                  (goto-char (point-min))
                  (forward-line 4)
                  (delete-region (point-min) (point))

                  ;; Display the buffer
                  (display-buffer output-buffer)
                  (format "Repo map generated successfully in buffer %s" buffer-name))
              (error "Failed to generate repo map (exit code %d): %s"
                     exit-code (buffer-string)))))))))

(defun opencode-edit-file (file-path old-string new-string &optional replace-all)
  "Replace OLD-STRING with NEW-STRING inside FILE-PATH.
When REPLACE-ALL is truthy, every occurrence is replaced."
  (unless (opencode--edit-permission-allowed-p)
    (error "File edits denied by `opencode-edit-permissions'"))
  (let* ((expanded-path (expand-file-name file-path))
         (replace-all (opencode--truthy replace-all)))
    (unless (file-exists-p expanded-path)
      (error "File does not exist: %s" expanded-path))
    (find-file-noselect expanded-path)
    (with-temp-buffer
      (insert-file-contents expanded-path)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (let ((count 0))
          (while (search-forward old-string nil t)
            (setq count (1+ count)))
          (cond
           ((= count 0)
            (error "Could not find text to replace in %s" file-path))
           ((and (> count 1) (not replace-all))
            (error "Found %d matches. Use replaceAll=true or give more context." count))
           (t
            (goto-char (point-min))
            (if replace-all
                (while (search-forward old-string nil t)
                  (replace-match new-string t t))
              (search-forward old-string)
              (replace-match new-string t t))
            (let ((inhibit-message t))
              (write-region (point-min) (point-max) expanded-path nil 'no-message))
            (let* ((msg (format "Successfully edited %s (%d replacement%s)"
                                file-path count (if (= count 1) "" "s")))
                   (diag (opencode--format-diagnostics
                          (opencode-treesit-get-diagnostics expanded-path))))
              (opencode--append-diagnostics msg diag)))))))))

;; Todo system implementation
(defvar opencode--todo-list nil
  "Current todo list for the session.")

(defun opencode-todowrite (todos)
  "Write/update the TODOS list for the current Emacs session."
  (setq opencode--todo-list todos)
  (let ((count (length todos)))
    (format "Updated todo list with %d item%s.\n\n%s"
            count (if (= count 1) "" "s")
            (opencode--todo->string todos))))

(defun opencode-todoread ()
  "Return the current todo list."
  (if opencode--todo-list
      (format "Current todo list:\n\n%s"
              (opencode--todo->string opencode--todo-list))
    "No todos are currently tracked."))

;; Additional helper functions for Emacs-specific tools

(defun opencode-list-buffers ()
  "List all open Emacs buffers."
  (mapcar 'buffer-name (buffer-list)))

(defun opencode-read-buffer (buffer)
  "Read contents of an Emacs BUFFER."
  (unless (buffer-live-p (get-buffer buffer))
    (error "Buffer %s is not live" buffer))
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun opencode-append-to-buffer (buffer text)
  "Append text to an Emacs BUFFER."
  (with-current-buffer (get-buffer-create buffer)
    (save-excursion
      (goto-char (point-max))
      (insert text)))
  (format "Appended text to buffer %s" buffer))

(defun opencode-list-directory (directory)
  "List contents of DIRECTORY."
  (let* ((target (or directory default-directory))
         (expanded (opencode--ensure-directory target))
         (entries (directory-files expanded nil nil t)))
    (mapconcat #'identity entries "\n")))

(defun opencode-make-directory (parent name)
  "Create a directory."
  (condition-case nil
      (progn
        (make-directory (expand-file-name name parent) t)
        (format "Directory %s created/verified in %s" name parent))
    (error (format "Error creating directory %s in %s" name parent))))

(defun opencode-create-file (path filename content)
  "Create or overwrite FILENAME inside PATH with CONTENT."
  (unless (opencode--edit-permission-allowed-p)
    (error "File creation denied by `opencode-edit-permissions'"))
  (let* ((directory (opencode--ensure-directory path))
         (full-path (expand-file-name filename directory)))
    (with-temp-buffer
      (insert content)
      (let ((inhibit-message t))
        (write-region (point-min) (point-max) full-path nil 'no-message)))
    (find-file-noselect full-path)
    (let* ((msg (format "Created file %s in %s" filename directory))
           (diag (opencode--format-diagnostics
                  (opencode-treesit-get-diagnostics full-path))))
      (opencode--append-diagnostics msg diag))))

(defun opencode-read-documentation (symbol)
  "Read documentation for a function or variable."
  (with-temp-message (format "Reading documentation for: %s" symbol)
    (condition-case err
        (let ((sym (intern symbol)))
          (cond
           ((fboundp sym)
            (documentation sym))
           ((boundp sym)
            (documentation-property sym 'variable-documentation))
           (t
            (format "No documentation found for %s" symbol))))
      (error (format "Error reading documentation for %s: %s"
                     symbol (error-message-string err))))))

(defun opencode-apply-diff-fenced (file-path diff-content &optional patch-options working-dir)
  "Apply a diff patch to a file."
  ;; Extract diff content from fenced blocks
  (let ((extracted-diff
         (if (string-match "```\\(?:diff\\|patch\\)?\n\\(\\(?:.\\|\n\\)*?\\)\n```" diff-content)
             (match-string 1 diff-content)
           ;; If no fenced block found, try to use content as-is but warn
           (progn
             (message "Warning: No fenced diff block found, using content as-is")
             diff-content))))

    ;; Continue with original logic using extracted diff
    (setq diff-content extracted-diff))

  (let ((original-default-directory default-directory)
        (user-patch-options (if (and patch-options (not (string-empty-p patch-options)))
                                (split-string patch-options " " t)
                              nil))
        ;; Combine user options with -N, ensuring -N is there.
        (base-options '("-N"))
        (effective-patch-options '()))

    (if user-patch-options
        (if (or (member "-N" user-patch-options) (member "--forward" user-patch-options))
            (setq effective-patch-options user-patch-options)
          (setq effective-patch-options (append user-patch-options base-options)))
      (setq effective-patch-options base-options))

    (let* ((out-buf-name (generate-new-buffer-name "*patch-stdout*"))
           (err-buf-name (generate-new-buffer-name "*patch-stderr*"))
           (target-file nil)
           (exit-status -1)
           (result-output "")
           (result-error ""))
      (unwind-protect
          (progn
            (when (and working-dir (not (string-empty-p working-dir)))
              (setq default-directory (expand-file-name working-dir)))

            (setq target-file (expand-file-name file-path))

            (unless (file-exists-p target-file)
              (error "File to patch does not exist: %s" target-file))

            (with-temp-message (format "Applying diff to: `%s` with options: %s" target-file effective-patch-options)
              (with-temp-buffer
                (insert diff-content)
                (unless (eq (char-before (point-max)) ?\n)
                  (goto-char (point-max))
                  (insert "\n"))

                (setq exit-status (apply #'call-process-region
                                         (point-min) (point-max)
                                         "patch"
                                         nil
                                         (list out-buf-name err-buf-name)
                                         nil
                                         (append effective-patch-options (list target-file))))))

            ;; Retrieve content from buffers
            (let ((stdout-buf (get-buffer out-buf-name))
                  (stderr-buf (get-buffer err-buf-name)))
              (when stdout-buf
                (with-current-buffer stdout-buf
                  (setq result-output (buffer-string))))
              (when stderr-buf
                (with-current-buffer stderr-buf
                  (setq result-error (buffer-string)))))

            (if (= exit-status 0)
                (let* ((base (format "Diff successfully applied to %s.\nPatch command options: %s\nPatch STDOUT:\n%s\nPatch STDERR:\n%s"
                                     target-file effective-patch-options result-output result-error))
                       (diag (opencode--format-diagnostics
                              (opencode-treesit-get-diagnostics target-file))))
                  (opencode--append-diagnostics base diag))
              (error "Failed to apply diff to %s (exit status %s).\nPatch command options: %s\nPatch STDOUT:\n%s\nPatch STDERR:\n%s"
                     target-file exit-status effective-patch-options result-output result-error)))
        ;; Cleanup
        (setq default-directory original-default-directory)
        (let ((stdout-buf-obj (get-buffer out-buf-name))
              (stderr-buf-obj (get-buffer err-buf-name)))
          (when (buffer-live-p stdout-buf-obj) (kill-buffer stdout-buf-obj))
          (when (buffer-live-p stderr-buf-obj) (kill-buffer stderr-buf-obj)))))))

(defun opencode-search-web (query)
  "Search the web using SearXNG."
  (with-temp-message (format "Searching for: `%s`" query)
    (let ((url (format "https://searx.stream/search?q=%s&format=json"
                       (url-hexify-string query))))
      (with-temp-buffer
        (url-insert-file-contents url)
        (let ((json-response (json-read)))
          (mapconcat (lambda (result)
                       (format "%s - %s\n%s"
                               (cdr (assoc 'title result))
                               (cdr (assoc 'url result))
                               (cdr (assoc 'content result))))
                     (cdr (assoc 'results json-response))
                     "\n\n"))))))

;; Tool definitions for gptel

(defun trafilatura-fetch-page (url)
  "Fetch and parse a web page into markdown format using Trafilatura."
  ;; run this command: uvx trafilatura --output-format=markdown --with-metadata -u the_url
  (with-temp-buffer
    (let ((exit-code (call-process "uvx" nil t nil
                        "trafilatura" "--output-format=markdown" "--with-metadata" "-u" url)))
      (if (= exit-code 0)
          (buffer-string)
        (error "Failed to fetch page: %s" (buffer-string))))))
(defvar opencode-tools
  (list
   ;; Enhanced existing tools
   (gptel-make-tool
    :function #'opencode-read-file
    :name "Read"
    :description opencode-read-file-description
    :args (list '(:name "filepath" :type string :description "Path to the file to read")
                '(:name "offset" :type number :description "Line number to start reading from (0-based)" :optional t)
                '(:name "limit" :type number :description "Number of lines to read" :optional t))
    :category "filesystem")

   (gptel-make-tool
    :function #'opencode-run-command
    :name "Bash"
    :description opencode-run-command-description
    :args (list '(:name "command" :type string :description "The shell command to execute")
                '(:name "working_dir" :type string :description "Working directory" :optional t)
                '(:name "timeout" :type number :description "Timeout in milliseconds" :optional t)
                '(:name "description" :type string :description "Brief description of what the command does" :optional t))
    :category "command"
    :confirm t)

   (gptel-make-tool
    :function #'opencode-edit-buffer
    :name "edit_buffer"
    :description "Enhanced Emacs buffer editing with better error handling"
    :args (list '(:name "buffer_name" :type string :description "Name of the buffer to modify")
                '(:name "old_string" :type string :description "Text to replace (must match exactly)")
                '(:name "new_string" :type string :description "Text to replace old_string with"))
    :category "emacs")

   ;; New tools from opencode
   (gptel-make-tool
    :function #'opencode-glob
    :name "Glob"
    :description opencode-glob-description
    :args (list '(:name "pattern" :type string :description "Glob pattern to match files")
                '(:name "path" :type string :description "Directory to search in. Can use ~ in the path" :optional t))
    :category "filesystem")

   (gptel-make-tool
    :function #'opencode-grep
    :name "Grep"
    :description opencode-grep-description
    :args (list '(:name "pattern" :type string :description "Regex pattern to search for")
                '(:name "include" :type string :description "File pattern to include" :optional t)
                '(:name "path" :type string :description "Directory to search in" :optional t))
    :category "filesystem")

   (gptel-make-tool
    :function #'opencode-edit-file
    :name "edit"
    :description opencode-edit-description
    :args (list '(:name "filePath" :type string :description "Path to the file to edit")
                '(:name "oldString" :type string :description "Text to replace")
                '(:name "newString" :type string :description "Replacement text")
                '(:name "replaceAll" :type boolean :description "Replace all occurrences" :optional t))
    :category "filesystem")

   (gptel-make-tool
    :function #'opencode-todowrite
    :name "todowrite"
    :description opencode-todowrite-description
    :args (list '(:name "todos" :type array :description "Array of todo items." :items (:type string)))
    :category "task")

   (gptel-make-tool
    :function #'opencode-todoread
    :name "todoread"
    :description opencode-todoread-description
    :args '()
    :category "task")

   ;; Emacs-specific tools
   (gptel-make-tool
    :function #'opencode-list-buffers
    :name "list_buffers"
    :description "Return a list of the names of all open Emacs buffers"
    :args '()
    :category "emacs")

   (gptel-make-tool
    :function #'opencode-read-buffer
    :name "read_buffer"
    :description "Return the contents of an Emacs buffer"
    :args (list '(:name "buffer" :type string :description "The name of the buffer whose contents are to be retrieved"))
    :category "emacs")

   (gptel-make-tool
    :function #'opencode-append-to-buffer
    :name "append_to_buffer"
    :description "Append text to an Emacs buffer. If the buffer does not exist, it will be created."
    :args (list '(:name "buffer" :type string :description "The name of the buffer to append text to")
                '(:name "text" :type string :description "The text to append to the buffer"))
    :category "emacs")

   ;; Additional filesystem tools
   (gptel-make-tool
    :function #'opencode-list-directory
    :name "LS"
    :description opencode-list-directory-description
    :args (list '(:name "directory" :type string :description "The path to the directory to list"))
    :category "filesystem")

   (gptel-make-tool
    :function #'opencode-make-directory
    :name "make_directory"
    :description "Create a new directory with the given name in the specified parent directory"
    :args (list '(:name "parent" :type string :description "The parent directory where the new directory should be created")
                '(:name "name" :type string :description "The name of the new directory to create"))
    :category "filesystem")

   (gptel-make-tool
    :function #'opencode-create-file
    :name "create_file"
    :description opencode-create-file-description
    :args (list '(:name "path" :type string :description "The directory where to create the file")
                '(:name "filename" :type string :description "The name of the file to create")
                '(:name "content" :type string :description "The content to write to the file"))
    :category "filesystem")

   (gptel-make-tool
    :function #'opencode-read-documentation
    :name "read_documentation"
    :description "Read the documentation for a given function or variable"
    :args (list '(:name "name" :type string :description "The name of the function or variable whose documentation is to be retrieved"))
    :category "emacs")

   (gptel-make-tool
    :function #'opencode-apply-diff-fenced
    :name "apply_diff_fenced"
    :description opencode-apply-diff-description
    :args (list '(:name "file_path" :type string :description "The path to the file that needs to be patched")
                '(:name "diff_content" :type string :description "The diff content within fenced code blocks in unified format")
                '(:name "patch_options" :type string :description "Optional: Additional options for the 'patch' command" :optional t)
                '(:name "working_dir" :type string :description "Optional: The directory in which to interpret file_path and run patch" :optional t))
    :category "filesystem")

   (gptel-make-tool
    :function #'aider-make-repo-map
    :name "aider_make_repo_map"
    :description "Create or refresh a repo map in an emacs buffer using aider's util."
    :args (list '(:name "path" :type string :description "The path to the repo to map"))
    :category "filesystem")

   (gptel-make-tool
    :function #'trafilatura-fetch-page
    :name "fetch_page"
    :description "Use Trafilatura to fetch and parse a web page into a markdown format."
    :args (list '(:name "url" :type string :description "The URL of the web page to fetch"))
    :category "web")

   (gptel-make-tool
    :function #'opencode-search-web
    :name "search_web"
    :description opencode-search-web-description
    :args (list '(:name "query" :type string :description "The search query to execute against the search engine"))
    :category "web"))
  "List of all opencode tools.")

(defconst opencode-all-tool-names
  (mapcar (lambda (tool) (opencode--get-tool-prop tool :name)) opencode-tools)
  "Names of all opencode tools in registration order.")

(defconst opencode-minimal-tool-names '("Read" "Bash" "LS")
  "Minimal tool preset for lightweight usage.")

(defconst opencode-essential-tool-names
  '("Read" "Bash" "LS" "edit" "apply_diff_fenced" "create_file" "todowrite" "todoread")
  "Essential tools suited for day-to-day editing tasks.")

(defconst opencode-coding-tool-names
  '("Read" "Bash" "Glob" "Grep" "edit" "apply_diff_fenced" "todowrite" "todoread"
    "LS" "create_file" "read_documentation" "search_web")
  "Coding-focused preset including search, editing, and planning tools.")

(defun opencode--tool-by-name (name)
  "Return the tool (plist or struct) matching NAME."
  (seq-find (lambda (tool) (string= (opencode--get-tool-prop tool :name) name)) opencode-tools))

(defun opencode--tool-present-p (name)
  "Return non-nil when NAME is already present in `gptel-tools'."
  (seq-some (lambda (tool) (string= (opencode--get-tool-prop tool :name) name)) gptel-tools))

(defun opencode--register-tools-by-names (names)
  "Register tools listed in NAMES with gptel."
  (dolist (name names)
    (when-let ((tool (opencode--tool-by-name name)))
      (unless (opencode--tool-present-p name)
        (setq gptel-tools (append gptel-tools (list tool)))))))

(defun opencode-register-tools ()
  "Register all opencode tools with gptel."
  (opencode--register-tools-by-names opencode-all-tool-names))

(defun opencode-register-minimal-tools ()
  "Register the minimal opencode tool set with gptel."
  (opencode--register-tools-by-names opencode-minimal-tool-names))

(defun opencode-register-essential-tools ()
  "Register the essential opencode tool set with gptel."
  (opencode--register-tools-by-names opencode-essential-tool-names))

(defun opencode-register-coding-tools ()
  "Register the coding-focused opencode tool set with gptel."
  (opencode--register-tools-by-names opencode-coding-tool-names))

(defun opencode-register-selected-tools (tool-names)
  "Register TOOL-NAMES with gptel."
  (opencode--register-tools-by-names tool-names))

(provide 'opencode-tools)

;;; opencode-tools.el ends here
