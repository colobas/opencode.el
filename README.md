# WARNING

This was heavily vibe-coded. I've since merged some PRs from public forks, but I definitely didn't plan on actively maintaining this, it was mostly an experiment.
If you're interested in taking the project over, feel free to reach out (see https://gpir.es for contacts) and I'll happily transfer ownership.

# opencode.el

A comprehensive port of [opencode](https://opencode.ai)'s sophisticated tools and agent system to Emacs via [gptel](https://github.com/karthink/gptel) integration.

## Table of Contents

- [Overview](#overview)
- [Architecture](#architecture)
- [Core Features](#core-features)
- [Installation](#installation)
- [Configuration](#configuration)
- [Usage](#usage)
- [Tool Reference](#tool-reference)
- [LSP Integration](#lsp-integration)
- [Agent System](#agent-system)
- [Permission System](#permission-system)
- [Migration Guide](#migration-guide)
- [Troubleshooting](#troubleshooting)
- [Contributing](#contributing)

## Overview

opencode.el brings the power of opencode's advanced LLM tooling to Emacs, providing a seamless integration between AI assistance and your development workflow. Unlike simple chat interfaces, opencode.el provides sophisticated tools that understand your codebase, integrate with language servers, and maintain context across complex multi-step tasks.

### What Makes opencode.el Different

- **Rich Tool Descriptions**: Each tool includes comprehensive usage instructions that guide LLM behavior
- **Deep Emacs Integration**: Native buffer operations, LSP integration, and Emacs-specific functionality
- **Intelligent Code Understanding**: LSP-powered error detection, symbol search, and real-time diagnostics
- **Task Management**: Structured todo system for complex, multi-step development tasks
- **Permission System**: Safe command execution with configurable security policies
- **Agent Specialization**: Different presets optimized for coding, research, or general tasks

## Architecture

opencode.el follows a modular architecture that cleanly separates concerns while providing seamless integration:

```
┌─────────────────────────────────────────────────────────────┐
│                        User Interface                       │
│                    (Emacs + gptel)                         │
└─────────────────────┬───────────────────────────────────────┘
                      │
┌─────────────────────▼───────────────────────────────────────┐
│                   opencode.el                              │
│                 (Main Package)                             │
│  ┌─────────────────┬─────────────────┬─────────────────┐   │
│  │   Setup &       │   Agent         │   Integration   │   │
│  │   Autoloads     │   Presets       │   Functions     │   │
│  └─────────────────┴─────────────────┴─────────────────┘   │
└─────────────────────┬───────────────────────────────────────┘
                      │
┌─────────────────────▼───────────────────────────────────────┐
│                Tool Layer                                   │
│  ┌─────────────────┬─────────────────┬─────────────────┐   │
│  │ opencode-tools  │ opencode-lsp    │ opencode-agents │   │
│  │                 │                 │                 │   │
│  │ • File Ops      │ • Diagnostics   │ • System        │   │
│  │ • Commands      │ • Symbols       │   Prompts       │   │
│  │ • Search        │ • Hover Info    │ • Presets       │   │
│  │ • Editing       │ • Auto-start    │ • Specialization│   │
│  │ • Tasks         │                 │                 │   │
│  └─────────────────┴─────────────────┴─────────────────┘   │
└─────────────────────┬───────────────────────────────────────┘
                      │
┌─────────────────────▼───────────────────────────────────────┐
│              Description Layer                              │
│                opencode-descriptions                        │
│                                                             │
│  Rich, comprehensive tool descriptions that guide LLM      │
│  behavior with usage patterns, constraints, and examples   │
└─────────────────────┬───────────────────────────────────────┘
                      │
┌─────────────────────▼───────────────────────────────────────┐
│                Integration Layer                            │
│  ┌─────────────────┬─────────────────┬─────────────────┐   │
│  │     gptel       │   lsp-mode      │   Emacs Core    │   │
│  │                 │                 │                 │   │
│  │ • Tool System   │ • Language      │ • Buffer Ops    │   │
│  │ • LLM Backends  │   Servers       │ • File System   │   │
│  │ • Streaming     │ • Diagnostics   │ • Process Mgmt  │   │
│  │ • Presets       │ • Symbols       │ • Async Ops     │   │
│  └─────────────────┴─────────────────┴─────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

### Component Interaction

1. **User Interface**: Standard gptel interface enhanced with opencode tools and presets
2. **Main Package**: Coordinates setup, configuration, and integration between components
3. **Tool Layer**: Implements sophisticated tools with rich functionality
4. **Description Layer**: Provides comprehensive guidance for LLM tool usage
5. **Integration Layer**: Connects to Emacs internals, LSP, and external systems

### Data Flow

```
User Input → gptel → opencode Tools → Emacs/LSP/System → Results → LLM → User
     ↑                                                                    ↓
     └─────────────── Rich Context & Tool Descriptions ──────────────────┘
```

## Core Features

### 🛠️ Enhanced File Operations

- **Intelligent Reading**: Line-numbered output with LSP integration for code understanding
- **Sophisticated Editing**: Multiple replacement strategies with real-time error detection
- **Pattern Matching**: Fast Glob and grep operations using ripgrep
- **Safe Creation**: File creation with automatic LSP startup and validation

### 🧠 LSP-Powered Intelligence

- **Real-time Diagnostics**: Automatic error detection after file operations
- **Symbol Navigation**: Workspace-wide symbol search and code understanding
- **Language Server Integration**: Automatic startup and management of language servers
- **Code Intelligence**: Hover information, document symbols, and semantic analysis

### 📋 Task Management

- **Structured Todos**: JSON-based task tracking with status management
- **Progress Visibility**: Real-time updates on complex multi-step tasks
- **Priority Management**: High/medium/low priority task organization
- **State Tracking**: Pending, in-progress, completed, and cancelled states

### 🔒 Security & Permissions

- **Command Filtering**: Configurable allow/deny/ask policies for shell commands
- **File Protection**: Permission checks for file editing operations
- **Safe Defaults**: Conservative security settings with user override options
- **Audit Trail**: Clear visibility into what operations are being performed

### 🤖 Agent Specialization

- **Coding Agent**: Optimized for software development with LSP tools
- **General Agent**: Research and analysis focused with comprehensive tools
- **Minimal Agent**: Essential tools only for lightweight usage
- **Custom Agents**: User-configurable tool sets and system prompts

## Installation

### Prerequisites

```bash
# Required
brew install ripgrep  # or apt install ripgrep

# Optional but recommended for full functionality
# LSP servers for your languages (e.g., typescript-language-server, pylsp, etc.)
```

### Emacs Packages

```elisp
;; Required
(use-package gptel
  :ensure t)

;; Optional but recommended
(use-package lsp-mode
  :ensure t)
```

### Install opencode.el

#### Manual Installation

```bash
git clone https://github.com/colobas/opencode.el.git ~/.emacs.d/opencode.el
```

```elisp
(add-to-list 'load-path "~/.emacs.d/opencode.el")
(require 'opencode)
(opencode-setup)
```

#### Using use-package

```elisp
(use-package opencode
  :vc (:url "https://github.com/colobas/opencode.el"
            :rev :newest
            :branch "main")
  :after gptel
  :config
  (opencode-setup-coding))  ; or opencode-setup, opencode-setup-minimal
```

#### Using straight.el

```elisp
(use-package opencode
  :straight (:host github :repo "colobas/opencode.el")
  :after gptel
  :config
  (opencode-setup))
```

## Configuration

### Basic Setup

```elisp
;; Full opencode experience
(opencode-setup)

;; Coding-focused (recommended for development)
(opencode-setup-coding)

;; Minimal tools only
(opencode-setup-minimal)

;; Custom configuration
(setq opencode-enabled-tools 'coding)
(setq opencode-default-preset 'opencode-coding)
(opencode-setup-custom)
```

### Permission System

```elisp
;; Configure command permissions
(setq opencode-bash-permissions 
      '(("ls*" . "allow")        ; Allow ls commands
        ("git*" . "allow")       ; Allow git commands
        ("npm*" . "allow")       ; Allow npm commands
        ("rm*" . "ask")          ; Ask before rm commands
        ("sudo*" . "deny")       ; Deny sudo commands
        ("*" . "ask")))          ; Ask for everything else

;; Configure file edit permissions
(setq opencode-edit-permissions "ask")  ; "allow", "deny", or "ask"
```

### LSP Integration

```elisp
;; Enable LSP features (default: enabled)
(setq opencode-lsp-enabled t)

;; Show diagnostics in tool output (default: enabled)
(setq opencode-lsp-show-diagnostics t)

;; Auto-start LSP servers when needed (default: enabled)
(setq opencode-lsp-auto-start t)
```

### Tool Selection

```elisp
;; Choose which tools to enable
(setq opencode-enabled-tools 'all)      ; All tools
(setq opencode-enabled-tools 'coding)   ; Coding-focused
(setq opencode-enabled-tools 'essential) ; Essential only
(setq opencode-enabled-tools '("Read" "edit" "Bash")) ; Custom list

;; Set default preset
(setq opencode-default-preset 'opencode-coding)
```

## Usage

### Starting a Session

```elisp
;; Start gptel with opencode tools
M-x gptel

;; Or use specific preset
M-x gptel-menu  ; then select opencode preset
```

### Key Bindings

The updated configuration adds opencode-specific bindings:

```elisp
SPC l o  ; opencode-setup (full)
SPC l c  ; opencode-setup-coding  
SPC l n  ; opencode-setup-minimal
SPC l b  ; gptel buffer
SPC l m  ; gptel menu
SPC l s  ; gptel send
```

### Example Interactions

**File Analysis:**
```
User: Analyze the main.py file and suggest improvementsAssistant: I'll read the main.py file and analyze it for potential improvements.

[Uses Read tool to read main.py with line numbers]
[Uses lsp_diagnostics tool to check for errors]
[Provides detailed analysis with specific suggestions]
```

**Complex Refactoring:**
```
User: Refactor the authentication system to use JWT tokensAssistant: I'll help you refactor the authentication system to use JWT tokens. Let me break this down into manageable tasks.

[Uses todowrite to create structured task list]
[Uses Glob to find authentication-related files]
[Uses grep to search for current auth patterns]
[Uses edit to implement JWT changes step by step]
[Shows LSP diagnostics after each change]
```

**Bug Investigation:**
```
User: There's a memory leak in the user service, can you find it?Assistant: I'll investigate the memory leak in the user service systematically.

[Uses lsp_symbols to find user service components]
[Uses grep to search for memory allocation patterns]
[Uses Read to examine suspicious code sections]
[Provides detailed analysis of potential leak sources]
```

## Tool Reference

### File Operations

| Tool | Description | LSP Integration |
|------|-------------|-----------------|
| `Read` | Read files with line numbers and offset/limit support | ✅ Warms LSP client |
| `edit` | Sophisticated string replacement with multiple strategies | ✅ Shows diagnostics after edit |
| `create_file` | Create new files with content validation | ✅ Auto-starts LSP |
| `list_directory` | List directory contents with enhanced descriptions | - |
| `apply_diff_fenced` | Apply unified diff patches to files | - |

### Search & Navigation

| Tool | Description | Backend |
|------|-------------|---------|
| `Glob` | Fast file pattern matching | find/ripgrep |
| `grep` | Content search with regex support | ripgrep |
| `lsp_symbols` | Workspace symbol search | LSP |

### Command Execution

| Tool | Description | Security |
|------|-------------|----------|
| `Bash` | Execute shell commands with timeout | ✅ Permission system |

### Task Management

| Tool | Description | Format |
|------|-------------|--------|
| `todowrite` | Create and update structured task lists | JSON |
| `todoread` | Read current task list | JSON |

### Emacs Integration

| Tool | Description | Native |
|------|-------------|--------|
| `read_buffer` | Read Emacs buffer contents | ✅ |
| `edit_buffer` | Edit Emacs buffers with error handling | ✅ |
| `append_to_buffer` | Append text to buffers | ✅ |
| `list_buffers` | List all open buffers | ✅ |
| `read_documentation` | Access Emacs function/variable docs | ✅ |

### Web & External

| Tool | Description | Backend |
|------|-------------|---------|
| `search_web` | Web search with formatted results | SearXNG |

## LSP Integration

### Architecture

opencode.el's LSP integration (`opencode-lsp.el`) provides a bridge between opencode tools and Emacs' lsp-mode:

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│  opencode Tool  │───▶│  opencode-lsp   │───▶│    lsp-mode     │
│                 │    │                 │    │                 │
│ • Read     │    │ • touch_file    │    │ • Language      │
│ • edit          │    │ • diagnostics   │    │   Servers       │
│ • create_file   │    │ • symbols       │    │ • Workspace     │
│                 │    │ • formatting    │    │ • Diagnostics   │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

### Features

1. **Automatic LSP Startup**: Language servers start automatically when files are accessed
2. **Real-time Diagnostics**: Errors and warnings appear in tool output
3. **Symbol Intelligence**: Workspace-wide symbol search and navigation
4. **Code Understanding**: Enhanced context for LLM through LSP information

### Configuration

```elisp
;; Enable/disable LSP integration
(setq opencode-lsp-enabled t)

;; Control diagnostic display
(setq opencode-lsp-show-diagnostics t)

;; Auto-start language servers
(setq opencode-lsp-auto-start t)
```

### Supported Operations

- **File Reading**: Warms LSP client for better subsequent operations
- **File Editing**: Shows compilation errors and warnings after changes
- **File Creation**: Starts appropriate language server for new files
- **Symbol Search**: Find functions, classes, variables across workspace
- **Diagnostics**: Get real-time error information for any file

## Agent System

### Preset Architecture

opencode.el provides specialized agent presets via gptel's preset system:

```elisp
;; Agent presets are defined using gptel-make-preset
(gptel-make-preset 'opencode-coding
  :description "Optimized for coding tasks"
  :system opencode-coding-system-prompt
  :tools '("Read" "edit" "Bash" "lsp_diagnostics" ...))
```

### Available Presets

#### `opencode` (Full Experience)
- **Purpose**: Complete opencode functionality
- **Tools**: All available tools including LSP, web search, and task management
- **System Prompt**: Comprehensive coding assistant with task management
- **Best For**: Complex development projects, research, and multi-step tasks

#### `opencode-coding` (Development Focused)
- **Purpose**: Optimized for software development
- **Tools**: File operations, LSP tools, command execution, task management
- **System Prompt**: Coding-focused with emphasis on code quality and testing
- **Best For**: Daily development work, debugging, refactoring

#### `opencode-general` (Research & Analysis)
- **Purpose**: Information gathering and analysis
- **Tools**: File reading, search tools, web search, task management
- **System Prompt**: Research-focused with systematic approach
- **Best For**: Code analysis, documentation, investigation tasks

#### `opencode-minimal` (Lightweight)
- **Purpose**: Essential functionality only
- **Tools**: Basic file operations and command execution
- **System Prompt**: Concise and direct
- **Best For**: Simple tasks, resource-constrained environments

### Custom Presets

Create your own specialized presets:

```elisp
(gptel-make-preset 'my-custom-preset
  :description "Custom preset for my workflow"
  :system "You are a specialized assistant for..."
  :tools '("Read" "edit" "Bash" "lsp_symbols"))
```

## Permission System

### Security Model

opencode.el implements a comprehensive permission system to ensure safe operation:

```
User Request → Tool Invocation → Permission Check → Execution/Denial
                                        ↓
                              ┌─────────────────┐
                              │ Permission      │
                              │ Policies        │
                              │                 │
                              │ • allow         │
                              │ • deny          │
                              │ • ask           │
                              └─────────────────┘
```

### Command Permissions

Configure shell command execution policies:

```elisp
(setq opencode-bash-permissions 
      '(("ls*" . "allow")        ; Allow listing commands
        ("git*" . "allow")       ; Allow git operations
        ("npm*" . "allow")       ; Allow npm commands
        ("pip*" . "allow")       ; Allow pip commands
        ("rm*" . "ask")          ; Confirm deletions
        ("sudo*" . "deny")       ; Block privileged commands
        ("curl*" . "ask")        ; Confirm network requests
        ("wget*" . "ask")        ; Confirm downloads
        ("*" . "ask")))          ; Ask for everything else
```

### File Edit Permissions

Control file modification operations:

```elisp
;; Global file edit policy
(setq opencode-edit-permissions "ask")  ; "allow", "deny", or "ask"

;; Per-directory policies (future enhancement)
(setq opencode-edit-directory-permissions
      '(("/tmp/*" . "allow")
        ("/etc/*" . "deny")
        ("~/.config/*" . "ask")))
```

### Permission Patterns

- **Glob Patterns**: Use shell-style wildcards (`*`, `?`, `[...]`)
- **Precedence**: More specific patterns override general ones
- **Default Policy**: The `"*"` pattern sets the default behavior

### Security Best Practices

1. **Start Restrictive**: Begin with `"ask"` for most operations
2. **Allow Common Operations**: Permit safe, frequently-used commands
3. **Deny Dangerous Operations**: Block privileged and destructive commands
4. **Regular Review**: Periodically audit and update permission policies

## Migration Guide

### From llm.el

If you're migrating from the original llm.el configuration:

#### Step 1: Backup Current Configuration
```elisp
;; Save your current llm.el configuration
cp ~/.emacs.d/llm.el ~/.emacs.d/llm.el.backup
```

#### Step 2: Install opencode.el
```elisp
;; Add opencode.el to your configuration
(use-package opencode
  :load-path "~/.emacs.d/opencode.el"
  :after gptel
  :config
  (opencode-setup-coding))
```

#### Step 3: Update llm.el
Replace your llm.el with the updated version that focuses on LLM backend configuration and integrates with opencode.el.

#### Step 4: Verify Integration
```elisp
;; Test that tools are working
M-x gptel
;; In the gptel buffer, try: "List the files in the current directory"
```

### Tool Migration Map

| Original llm.el Tool | opencode.el Equivalent | Enhancements |
|---------------------|------------------------|--------------|
| `Read` | `Read` | ✅ Line numbers, LSP integration |
| `Bash` | `Bash` | ✅ Permission system, security |
| `edit_buffer` | `edit_buffer` | ✅ Better error handling |
| `create_file` | `create_file` | ✅ LSP integration, validation |
| `apply_diff_fenced` | `apply_diff_fenced` | ✅ Enhanced error handling |
| - | `Glob` | ✨ **NEW**: Pattern matching |
| - | `grep` | ✨ **NEW**: Content search |
| - | `edit` | ✨ **NEW**: Sophisticated editing |
| - | `todowrite`/`todoread` | ✨ **NEW**: Task management |
| - | `lsp_diagnostics` | ✨ **NEW**: Error detection |
| - | `lsp_symbols` | ✨ **NEW**: Symbol search |

## Troubleshooting

### Common Issues

#### LSP Not Working
```elisp
;; Check if lsp-mode is installed
(featurep 'lsp-mode)  ; Should return t

;; Verify LSP is enabled
opencode-lsp-enabled  ; Should be t

;; Check if language server is available
M-x lsp-doctor
```

#### Permission Errors
```elisp
;; Check permission configuration
opencode-bash-permissions

;; Temporarily allow all commands (not recommended for production)
(setq opencode-bash-permissions '(("*" . "allow")))
```

#### Tools Not Available
```elisp
;; Verify opencode is loaded
(featurep 'opencode)  ; Should return t

;; Check tool registration
(length gptel-tools)  ; Should show multiple tools

;; Re-register tools
(opencode-setup)
```

#### Ripgrep Not Found
```bash
# Install ripgrep
brew install ripgrep  # macOS
sudo apt install ripgrep  # Ubuntu/Debian
sudo dnf install ripgrep  # Fedora
```

### Debug Mode

Enable detailed logging for troubleshooting:

```elisp
;; Enable gptel debug logging
(setq gptel-log-level 'debug)

;; Check opencode configuration
(opencode-debug-info)  ; Custom debug function
```

### Performance Issues

If tools are slow:

```elisp
;; Disable LSP integration temporarily
(setq opencode-lsp-enabled nil)

;; Use minimal tool set
(opencode-setup-minimal)

;; Check ripgrep performance
(benchmark-run 10 (opencode-grep "function" "*.py"))
```

## Contributing

### Development Setup

```bash
# Clone the repository
git clone https://github.com/colobas/opencode.el.git
cd opencode.el

# Install development dependencies
# (Add any specific development setup here)
```

### Code Style

- Follow Emacs Lisp conventions
- Use `lexical-binding: t`
- Include comprehensive docstrings
- Add type hints where appropriate
- Follow the existing code organization

### Testing

```elisp
;; Run tests (when test suite is available)
M-x ert RET opencode-test-* RET

;; Manual testing
(opencode-test-tool-integration)
```

### Submitting Changes

1. **Fork** the repository
2. **Create** a feature branch
3. **Add** tests for new functionality
4. **Update** documentation
5. **Submit** a pull request

### Areas for Contribution

- **Additional LSP Features**: Hover information, code actions, formatting
- **More Tool Integrations**: Git operations, package managers, build systems
- **Performance Optimizations**: Caching, async operations, lazy loading
- **Security Enhancements**: Sandboxing, audit logging, policy management
- **Documentation**: Examples, tutorials, best practices
- **Testing**: Unit tests, integration tests, performance benchmarks

## Attribution

This package uses code from [opencode](https://github.com/sst/opencode) which is licensed under the MIT License.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- **[opencode](https://opencode.ai)** - Original tool system and sophisticated LLM integration
- **[gptel](https://github.com/karthink/gptel)** - Excellent Emacs LLM integration framework
- **[lsp-mode](https://github.com/emacs-lsp/lsp-mode)** - Comprehensive LSP support for Emacs
- **The Emacs Community** - Feedback, contributions, and continuous improvement

---

**opencode.el** brings the power of sophisticated LLM tooling to your Emacs workflow, enabling AI-assisted development that understands your code, respects your security, and scales with your projects.
