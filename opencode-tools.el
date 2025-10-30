;;; opencode-tools.el --- OpenCode tools for gptel -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains tool implementations using gptel-make-tool.
;; It includes both enhanced versions of existing tools from llm.el
;; and new tools ported from opencode.

;;; Code:

(require 'gptel)
(require 'opencode-descriptions)
(require 'opencode-treesit)

;; Permission system variables
(defcustom opencode-bash-permissions '(("*" . "ask"))
  "Permission settings for bash commands.
Each entry is (PATTERN . ACTION) where:
- PATTERN is a glob pattern matching commands
- ACTION is 'allow', 'deny', or 'ask'"
  :type '(alist :key-type string :value-type (choice (const allow) (const deny) (const ask)))
  :group 'opencode)

(defcustom opencode-edit-permissions "ask"
  "Permission setting for file edits.
Can be 'allow', 'deny', or 'ask'."
  :type '(choice (const allow) (const deny) (const ask))
  :group 'opencode)

;; Helper functions
(defun opencode--check-permission (command permissions)
  "Check if COMMAND is allowed based on PERMISSIONS."
  (let ((action (cdr (assoc "*" permissions))))
    (dolist (entry permissions)
      (when (string-match-p (car entry) command)
        (setq action (cdr entry))))
    (cond
     ((eq action 'allow) t)
     ((eq action 'deny) nil)
     ((eq action 'ask) (y-or-n-p (format "Execute command: %s? " command)))
     (t nil))))

(defun opencode--add-line-numbers (content)
  "Add line numbers to CONTENT in cat -n format."
  (let ((lines (split-string content "\n"))
        (line-num 1)
        result)
    (dolist (line lines)
      (push (format "%6d\t%s" line-num line) result)
      (setq line-num (1+ line-num)))
    (string-join (nreverse result) "\n")))

;; Enhanced tools from llm.el with opencode descriptions

(defun opencode-read-file (filepath &optional offset limit)
  "Read file with line numbers, supporting offset and limit.
Enhanced with tree-sitter syntax analysis when available."
  (let ((expanded-path (expand-file-name filepath)))
    (unless (file-exists-p expanded-path)
      (error "File does not exist: %s" expanded-path))
    ;; Touch file for LSP (warm up client)
    (find-file-noselect expanded-path)
    (with-temp-buffer
      (insert-file-contents expanded-path)
      (let* ((lines (split-string (buffer-string) "\n"))
             (start (or offset 0))
             (end (if limit (min (+ start limit) (length lines)) (length lines)))
             (selected-lines (cl-subseq lines start end))
             (numbered-content (mapconcat
                               (lambda (line)
                                 (format "%6d\t%s" (+ start (cl-position line selected-lines :test #'equal) 1) line))
                               selected-lines "\n"))
             ;; Add tree-sitter diagnostics if available
             (diagnostics (opencode-treesit-get-diagnostics expanded-path)))
        (if diagnostics
            (concat numbered-content "\n\n--- Tree-sitter Analysis ---\n"
                    (mapconcat
                     (lambda (diag)
                       (format "Line %d:%d-%d:%d | %s | %s | Source: %s"
                               (plist-get diag :line)
                               (plist-get diag :column)
                               (plist-get diag :end-line)
                               (plist-get diag :end-column)
                               (plist-get diag :severity)
                               (plist-get diag :message)
                               (plist-get diag :source)))
                     diagnostics "\n"))
          numbered-content)))))

(defun opencode-run-command (command &optional working-dir timeout description)
  "Execute shell command with permission checking and enhanced output."
  (unless (opencode--check-permission command opencode-bash-permissions)
    (error "Command execution denied: %s" command))
  (let ((default-directory (if (and working-dir (not (string-empty-p working-dir)))
                              (expand-file-name working-dir)
                            default-directory))
        (process-environment process-environment))
    (with-temp-message (format "Executing: %s" (or description command))
      (shell-command-to-string command))))

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
  "Fast file pattern matching using find command."
  (let ((search-path (expand-file-name (or path default-directory))))
    (with-temp-buffer
      (let ((exit-code (call-process "find" nil t nil
                                    search-path
                                    "-name" pattern
                                    "-type" "f")))
        (if (= exit-code 0)
            (let ((files (split-string (buffer-string) "\n" t)))
              (string-join (sort files #'string<) "\n"))
          (error "Glob pattern search failed"))))))

(defun opencode-grep (pattern &optional include path)
  "Fast content search using ripgrep."
  (let ((search-path (expand-file-name (or path default-directory)))
        (rg-args (list "--line-number" "--with-filename" "--color=never")))
    (when include
      (setq rg-args (append rg-args (list "--glob" include))))
    (setq rg-args (append rg-args (list pattern search-path)))
    (with-temp-buffer
      (let ((exit-code (apply #'call-process "rg" nil t nil rg-args)))
        (if (= exit-code 0)
            (buffer-string)
          (if (= exit-code 1)
              "No matches found"
            (error "Grep search failed with exit code %d" exit-code)))))))

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
  "Sophisticated file editing with multiple strategies."
  (let ((expanded-path (expand-file-name file-path)))
    (unless (file-exists-p expanded-path)
      (error "File does not exist: %s" expanded-path))
    ;; Touch file for LSP and get tree-sitter analysis
    (find-file-noselect expanded-path)
    (let ((treesit-diagnostics (opencode-treesit-get-diagnostics expanded-path)))
    (with-temp-buffer
      (insert-file-contents expanded-path)
      (let ((original-content (buffer-string))
            (case-fold-search nil)
            (count 0))
        ;; Count matches
        (goto-char (point-min))
        (while (search-forward old-string nil t)
          (setq count (1+ count)))

        (cond
         ((= count 0)
          (error "Could not find text to replace in file %s" file-path))
         ((and (> count 1) (not replace-all))
          (error "Found %d matches. Use replaceAll=true or provide more context to make the match unique" count))
         (t
          ;; Perform replacement
          (goto-char (point-min))
          (if replace-all
              (while (search-forward old-string nil t)
                (replace-match new-string t t))
            (search-forward old-string)
            (replace-match new-string t t))
          ;; Write back to file
          (write-region (point-min) (point-max) expanded-path)
          (let ((base-output (format "Successfully edited file %s (%d replacement%s)"
                                     file-path count (if (= count 1) "" "s"))))
            (let ((diagnostics (opencode-treesit-get-diagnostics expanded-path)
                                  ))
              (if diagnostics
                  (concat base-output "\n\n--- Analysis ---\n" diagnostics)
                base-output))))))))))

;; Todo system implementation
(defvar opencode--todo-list nil
  "Current todo list for the session.")

(defun opencode-todowrite (todos)
  "Write/update the TODOS list."
  (setq opencode--todo-list todos)
  (format "Updated todo list with %d items" (length todos)))

(defun opencode-todoread ()
  "Read the current todo list."
  (or opencode--todo-list '()))

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
  "List contents of a directory."
  (mapconcat #'identity (directory-files directory) "\n"))

(defun opencode-make-directory (parent name)
  "Create a directory."
  (condition-case nil
      (progn
        (make-directory (expand-file-name name parent) t)
        (format "Directory %s created/verified in %s" name parent))
    (error (format "Error creating directory %s in %s" name parent))))

(defun opencode-create-file (path filename content)
  "Create a new file with specified content."
  (let ((full-path (expand-file-name filename path)))
    (with-temp-buffer
      (insert content)
      (write-file full-path))
    ;; Touch file for LSP
    (find-file-noselect full-path)
    (let ((base-output (format "Created file %s in %s" filename path)))
      ;; Add diagnostics - prefer tree-sitter, fallback to LSP
      (let ((diagnostics (opencode-treesit-get-diagnostics full-path)
                            ))
        (if diagnostics
            (concat base-output "\n\n--- Analysis ---\n" diagnostics)
          base-output)))))

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
                (format "Diff successfully applied to %s.\nPatch command options: %s\nPatch STDOUT:\n%s\nPatch STDERR:\n%s"
                        target-file effective-patch-options result-output result-error)
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
    :args (list '(:name "pattern" :type string :description "Glob pattern to match files. (The `-name` argument of a `find` command")
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
    :args (list '(:name "todos" :type array :description "Array of todo items with id, content, status, priority" :items (:type string)))
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

(defun opencode-register-tools ()
  "Register all opencode tools with gptel."
  (setq gptel-tools (append gptel-tools opencode-tools)))

(provide 'opencode-tools)

;;; opencode-tools.el ends here
