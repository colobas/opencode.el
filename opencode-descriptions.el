;;; opencode-descriptions.el --- Tool descriptions for opencode.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Descriptions are adapted from the upstream opencode project and tuned for
;; the Emacs + gptel environment.

;;; Code:

(defconst opencode-read-file-description
  "Read a file from disk and return numbered lines.

Overview:
- Accepts absolute or relative paths; relative paths resolve against Emacs’ current working directory.
- Optional `offset` (0-based) and `limit` let you page through large files; omit both to read the entire file.
- Lines are returned in a `cat -n` style format so you can reference them in later tool calls.

Guidance:
- Use this tool before editing so you work with fresh context.
- If you only need part of a large file, start with a smaller window, then increase `offset` to continue.
- Binary files are rejected—use operating-system tools instead when you truly need them.

Emacs integration:
- Files are opened with `find-file-noselect`, warming LSP/tree-sitter state for follow-up actions.
- When tree-sitter diagnostics are available they are appended after the file content."
  "Description for the Read tool.")

(defconst opencode-run-command-description
  "Run a shell command from Emacs with optional working directory, timeout, and human-readable description.

Parameters:
- `command` (required) is executed via your current `shell-file-name`.
- `working_dir` (optional) temporarily becomes the command’s working directory.
- `timeout` (optional) is in milliseconds (default 120000). The process is terminated if it exceeds the limit.
- `description` (optional) is echoed to the user to explain the intent of the command.

Permissions & safety:
- Every command is checked against `opencode-bash-permissions`. `allow` runs immediately, `ask` prompts for confirmation, and `deny` blocks the call.
- Keep destructive commands explicit (`rm path/to/file` instead of long pipelines) so the confirmation prompt is meaningful.
- Commands run non-interactively; avoid tools that require TTY interaction.

Usage notes:
- Prefer dedicated tools (`Read`, `LS`, `Glob`, `Grep`) for inspection, but fall back to Bash when you truly need shell semantics.
- Quote paths containing spaces or special characters.
- Chain related commands with `&&` or `;` on a single line. Multi-line shell scripts are not supported."
  "Description for the Bash tool.")

(defconst opencode-edit-description
  "Replace text in an existing file using exact string matching.

Workflow:
- Read the file first so you know the exact text you intend to replace.
- Provide `oldString` exactly as it appears in the file (without the line-number prefix emitted by `Read`).
- Set `replaceAll` to true when you intend to update every occurrence; otherwise the first unique match is replaced.

Safety:
- The command honours `opencode-edit-permissions`. Set it to `ask` when you want confirmation before edits.
- The edit fails when `oldString` cannot be found or matches multiple regions without `replaceAll`.
- Prefer editing over creating new files unless the user explicitly asks for new artifacts.

Emacs integration:
- The file is loaded into a buffer via `find-file-noselect`, then saved back to disk after the replacement.
- Tree-sitter diagnostics (when available) are appended so you can catch syntax issues immediately."
  "Description for the edit tool.")

(defconst opencode-glob-description
  "Locate files that match a glob pattern.

- Uses ripgrep’s `--files` mode when `rg` is available, falling back to `find` otherwise.
- `pattern` follows shell glob syntax (e.g. `src/**/*.ts`).
- `path` is optional; omit it to search from the current working directory.
- Results are sorted alphabetically and truncated to 100 entries with a notice when additional matches are omitted."
  "Description for the Glob tool.")

(defconst opencode-grep-description
  "Search file contents with ripgrep and return filename, line number, and the matching line.

- `pattern` is a regular expression understood by ripgrep.
- Use `include` to limit the search to specific globs (e.g. `*.py`).
- `path` changes the search root; omit it to search from the current directory.
- When no matches are found the tool reports that explicitly to avoid confusion."
  "Description for the Grep tool.")

(defconst opencode-todowrite-description
  "Store or update the current todo list for this Emacs session.

- Provide an array of todo objects (for example plists or alists) that include an id/content/status/priority field of your choosing.
- The list is cached in memory only; write it to disk yourself if you need permanence.
- Use this tool frequently to keep plans visible to the user and mark items complete as soon as you finish them."
  "Description for the todowrite tool.")

(defconst opencode-todoread-description
  "Return the todo list previously recorded with `todowrite`.

- Takes no parameters—call it with an empty argument list.
- Handy at the start of a session, before switching tasks, or whenever you need to restate priorities.
- Returns an empty list when no todos are currently tracked."
  "Description for the todoread tool.")

(defconst opencode-list-directory-description
  "List the immediate children of a directory using Emacs’ `directory-files`.

- The path may be absolute or relative to the current working directory.
- Hidden files are included; filter them yourself if you need a narrower view.
- Prefer `Glob` when you need recursive searches or pattern matching."
  "Description for the list_directory tool.")

(defconst opencode-create-file-description
  "Create or overwrite a file with the supplied content.

- Supply `path` (directory), `filename`, and the desired file contents.
- Read existing files before you overwrite them so you do not clobber unexpected changes.
- The command respects `opencode-edit-permissions`; set it to `ask` to confirm before writing.
- Tree-sitter diagnostics (when available) are appended after the write to highlight syntax issues."
  "Description for the create_file tool.")

(defconst opencode-apply-diff-description
  "Apply a unified diff to a file.

- Provide the diff inside a fenced ```diff or ```patch block; the wrapper is stripped automatically.
- `patch_options` are passed straight to the `patch` utility (default `-N` to ignore already-applied hunks).
- Use this when you need to modify several regions in one call or maintain precise context."
  "Description for the apply_diff tool.")

(defconst opencode-search-web-description
  "Query the SearXNG meta-search instance and format the results with title, URL, and snippet.

- Suitable for quick research without leaving Emacs.
- Results are returned in plain text; follow the URLs in a browser if you need the full page."
  "Description for the search_web tool.")

(provide 'opencode-descriptions)

;;; opencode-descriptions.el ends here
