;;; opencode-descriptions.el --- Tool descriptions ported from opencode -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains tool descriptions ported from opencode's .txt files.
;; These rich descriptions guide LLM behavior and provide comprehensive
;; usage instructions, best practices, and constraints.

;;; Code:

(defconst opencode-read-file-description
  "Reads a file from the local filesystem with LSP integration for enhanced code understanding.
Assume this tool is able to read all files on the machine. If the User provides a path to a file assume that path is valid. It is okay to read a file that does not exist; an error will be returned.

Usage:
- By default, it reads up to 2000 lines starting from the beginning of the file
- You can optionally specify a line offset and limit (especially handy for long files), but it's recommended to read the whole file by not providing these parameters
- Any lines longer than 2000 characters will be truncated
- Results are returned using cat -n format, with line numbers starting at 1
- This tool allows opencode to read images (eg PNG, JPG, etc). When reading an image file the contents are presented visually as opencode is a multimodal LLM.
- You have the capability to call multiple tools in a single response. It is always better to speculatively read multiple files as a batch that are potentially useful.
- You will regularly be asked to read screenshots. If the user provides a path to a screenshot ALWAYS use this tool to view the file at the path. This tool will work with all temporary file paths like /var/folders/123/abc/T/TemporaryItems/NSIRD_screencaptureui_ZfB1tD/Screenshot.png
- If you read a file that exists but has empty contents you will receive a system reminder warning in place of file contents.

LSP Integration:
- Automatically warms up the LSP client when reading code files
- Enables better code understanding and symbol resolution for subsequent operations"
  "Description for the Read tool.")

(defconst opencode-run-command-description
  "Executes a given bash command in a persistent shell session with optional timeout, ensuring proper handling and security measures.

Before executing the command, please follow these steps:

1. Directory Verification:
   - If the command will create new directories or files, first use the LS tool to verify the parent directory exists and is the correct location
   - For example, before running \"mkdir foo/bar\", first use LS to check that \"foo\" exists and is the intended parent directory

2. Command Execution:
   - Always quote file paths that contain spaces with double quotes (e.g., \"path with spaces/file.txt\")
   - Examples of proper quoting:
     - cd \"/Users/name/My Documents\" (correct)
     - cd /Users/name/My Documents (incorrect - will fail)
     - python \"/path/with spaces/script.py\" (correct)
     - python /path/with spaces/script.py (incorrect - will fail)
   - After ensuring proper quoting, execute the command.
   - Capture the output of the command.

Usage notes:
  - The command argument is required.
  - You can specify an optional timeout in milliseconds (up to 600000ms / 10 minutes). If not specified, commands will timeout after 120000ms (2 minutes).
  - It is very helpful if you write a clear, concise description of what this command does in 5-10 words.
  - If the output exceeds 30000 characters, output will be truncated before being returned to you.
  - VERY IMPORTANT: You MUST avoid using search commands like `find` and `grep`. Instead use Grep, Glob. You MUST avoid read tools like `cat`, `head`, `tail`, and `ls`, and use Read and LS to read files.
  - If you _still_ need to run `grep`, STOP. ALWAYS USE ripgrep at `rg` (or /usr/bin/rg) first, which all opencode users have pre-installed.
  - When issuing multiple commands, use the ';' or '&&' operator to separate them. DO NOT use newlines (newlines are ok in quoted strings).
  - Don't use `cd` unless the user requests it explicitly."
  "Description for the Bash tool.")

(defconst opencode-edit-description
  "Performs exact string replacements in files with LSP integration for error detection.

Usage:
- You must use your `Read` tool at least once in the conversation before editing. This tool will error if you attempt an edit without reading the file.
- When editing text from Read tool output, ensure you preserve the exact indentation (tabs/spaces) as it appears AFTER the line number prefix. The line number prefix format is: spaces + line number + tab. Everything after that tab is the actual file content to match. Never include any part of the line number prefix in the oldString or newString.
- ALWAYS prefer editing existing files in the codebase. NEVER write new files unless explicitly required.
- Only use emojis if the user explicitly requests it. Avoid adding emojis to files unless asked.
- The edit will FAIL if `oldString` is not unique in the file. Either provide a larger string with more surrounding context to make it unique or use `replaceAll` to change every instance of `oldString`.
- Use `replaceAll` for replacing and renaming strings across the file. This parameter is useful if you want to rename a variable for instance.

LSP Integration:
- Automatically runs LSP diagnostics after editing files
- Shows compilation errors, warnings, and other issues in the output
- Helps ensure code changes don't introduce syntax or semantic errors"
  "Description for the edit tool.")

(defconst opencode-glob-description
  "- Fast file pattern matching tool that works with any codebase size
- Supports glob patterns like \"*.js\" or \"src/**/*.ts\"
- Returns matching file paths sorted by modification time
- Use this tool when you need to find files by name patterns
- When you are doing an open ended search that may require multiple rounds of globbing and grepping, use the Agent tool instead
- You have the capability to call multiple tools in a single response. It is always better to speculatively perform multiple searches as a batch that are potentially useful."
  "Description for the Glob tool.")

(defconst opencode-grep-description
  "- Fast content search tool that works with any codebase size
- Searches file contents using regular expressions
- Supports full regex syntax (eg. \"log.*Error\", \"function\\s+\\w+\", etc.)
- Filter files by pattern with the include parameter (eg. \"*.js\", \"*.{ts,tsx}\")
- Returns file paths with at least one match sorted by modification time
- Use this tool when you need to find files containing specific patterns
- If you need to identify/count the number of matches within files, use the Bash tool with `rg` (ripgrep) directly. Do NOT use `grep`.
- When you are doing an open ended search that may require multiple rounds of globbing and grepping, use the Agent tool instead"
  "Description for the grep tool.")

(defconst opencode-todowrite-description
  "Use this tool to create and manage a structured task list for your current coding session. This helps you track progress, organize complex tasks, and demonstrate thoroughness to the user.
It also helps the user understand the progress of the task and overall progress of their requests.

## When to Use This Tool
Use this tool proactively in these scenarios:

1. Complex multi-step tasks - When a task requires 3 or more distinct steps or actions
2. Non-trivial and complex tasks - Tasks that require careful planning or multiple operations
3. User explicitly requests todo list - When the user directly asks you to use the todo list
4. User provides multiple tasks - When users provide a list of things to be done (numbered or comma-separated)
5. After receiving new instructions - Immediately capture user requirements as todos. Feel free to edit the todo list based on new information.
6. After completing a task - Mark it complete and add any new follow-up tasks
7. When you start working on a new task, mark the todo as in_progress. Ideally you should only have one todo as in_progress at a time. Complete existing tasks before starting new ones.

## When NOT to Use This Tool

Skip using this tool when:
1. There is only a single, straightforward task
2. The task is trivial and tracking it provides no organizational benefit
3. The task can be completed in less than 3 trivial steps
4. The task is purely conversational or informational

NOTE that you should not use this tool if there is only one trivial task to do. In this case you are better off just doing the task directly.

## Task States and Management

1. **Task States**: Use these states to track progress:
   - pending: Task not yet started
   - in_progress: Currently working on (limit to ONE task at a time)
   - completed: Task finished successfully
   - cancelled: Task no longer needed

2. **Task Management**:
   - Update task status in real-time as you work
   - Mark tasks complete IMMEDIATELY after finishing (don't batch completions)
   - Only have ONE task in_progress at any time
   - Complete current tasks before starting new ones
   - Cancel tasks that become irrelevant

3. **Task Breakdown**:
   - Create specific, actionable items
   - Break complex tasks into smaller, manageable steps
   - Use clear, descriptive task names

When in doubt, use this tool. Being proactive with task management demonstrates attentiveness and ensures you complete all requirements successfully."
  "Description for the todowrite tool.")

(defconst opencode-todoread-description
  "Use this tool to read the current to-do list for the session. This tool should be used proactively and frequently to ensure that you are aware of
the status of the current task list. You should make use of this tool as often as possible, especially in the following situations:
- At the beginning of conversations to see what's pending
- Before starting new tasks to prioritize work
- When the user asks about previous tasks or plans
- Whenever you're uncertain about what to do next
- After completing tasks to update your understanding of remaining work
- After every few messages to ensure you're on track

Usage:
- This tool takes in no parameters. So leave the input blank or empty. DO NOT include a dummy object, placeholder string or a key like \"input\" or \"empty\". LEAVE IT BLANK.
- Returns a list of todo items with their status, priority, and content
- Use this information to track progress and plan next steps
- If no todos exist yet, an empty list will be returned"
  "Description for the todoread tool.")

(defconst opencode-list-directory-description
  "Lists files and directories in a given path. The path parameter must be an absolute path, not a relative path. You can optionally provide an array of Glob patterns to ignore with the ignore parameter. You should generally prefer the Glob and Grep tools, if you know which directories to search."
  "Description for the list_directory tool.")

(defconst opencode-create-file-description
  "Writes a file to the local filesystem with LSP integration for error detection.

Usage:
- This tool will overwrite the existing file if there is one at the provided path.
- If this is an existing file, you MUST use the Read tool first to read the file's contents. This tool will fail if you did not read the file first.
- ALWAYS prefer editing existing files in the codebase. NEVER write new files unless explicitly required.
- NEVER proactively create documentation files (*.md) or README files. Only create documentation files if explicitly requested by the User.
- Only use emojis if the user explicitly requests it. Avoid writing emojis to files unless asked.

LSP Integration:
- Automatically starts LSP for the new file if applicable
- Shows compilation errors, warnings, and other issues in the output
- Helps ensure new files are syntactically and semantically correct"
  "Description for the create_file tool.")

(defconst opencode-apply-diff-description
  "Applies a diff (patch) to a specified file using fenced diff content. This is the PREFERRED method for modifying files as it is more token-efficient.
The diff must be provided within fenced code blocks (```diff or ```patch) and be in unified format.
The LLM should generate the diff such that the file paths within the diff
(e.g., '--- a/filename' '+++ b/filename') are appropriate for the 'file_path' argument and chosen 'patch_options'.
Common 'patch_options' include: '' (empty, if paths in diff are exact or relative to current dir of file_path),
'-p0' (if diff paths are full or exactly match the target including prefixes like 'a/'),
'-p1' (if diff paths have one leading directory to strip, e.g., diff has 'a/src/file.c' and you want to patch 'src/file.c' from project root).
Default options are '-N' (ignore already applied patches)."
  "Description for the apply_diff tool.")

(defconst opencode-search-web-description
  "Searches the web using SearXNG metasearch engine and returns formatted results including titles, URLs, and content excerpts."
  "Description for the search_web tool.")

(provide 'opencode-descriptions)

;;; opencode-descriptions.el ends here
