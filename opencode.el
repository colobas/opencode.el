;;; opencode.el --- OpenCode tools and agents for Emacs via gptel -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: opencode.el contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (gptel "0.9.0"))
;; Keywords: ai, llm, tools, coding
;; URL: https://github.com/opencode/opencode.el

;;; Commentary:

;; This package ports opencode's sophisticated tools and agent system to Emacs
;; via gptel integration. It provides:
;;
;; - Rich tool descriptions that guide LLM behavior
;; - Advanced file operations (Glob, Grep, edit with multiple strategies)
;; - Task management system (todowrite/todoread)
;; - Permission system for safe operations
;; - Agent presets with specialized system prompts
;;
;; Usage:
;;   (require 'opencode)
;;   (opencode-setup)  ; Full setup with all tools
;;   ;; or
;;   (opencode-setup-minimal)  ; Essential tools only

;;; Code:

(require 'cl-lib)
(require 'gptel)

;; Customization group
(defgroup opencode nil
  "OpenCode tools and agents for Emacs via gptel."
  :group 'tools
  :prefix "opencode-")

;; Load submodules
(require 'opencode-descriptions)
(require 'opencode-treesit)
(require 'opencode-tools)
(require 'opencode-agents)

;; Customization variables
(defcustom opencode-enabled-tools 'all
  "Which tools to enable.
Can be 'all, 'essential, 'coding, 'minimal, or a list of tool names."
  :type '(choice (const :tag "All tools" all)
                 (const :tag "Essential tools only" essential)
                 (const :tag "Coding-focused tools" coding)
                 (const :tag "Minimal tools" minimal)
                 (repeat :tag "Custom tool list" string))
  :group 'opencode)

(defcustom opencode-default-preset 'opencode
  "Default preset to use when setting up opencode."
  :type '(choice (const opencode)
                 (const opencode-coding)
                 (const opencode-general)
                 (const opencode-minimal))
  :group 'opencode)

;;;###autoload
(defun opencode-setup ()
  "Set up opencode tools and presets with gptel.
This enables the full opencode experience with all tools and agents."
  (interactive)
  (opencode-register-tools)
  (opencode-register-agents)
  (message "OpenCode setup complete. Use '%s' preset for full experience." opencode-default-preset))

;;;###autoload
(defun opencode-setup-minimal ()
  "Set up opencode with the minimal tool set."
  (interactive)
  (opencode-register-minimal-tools)
  (opencode-register-agents)
  (message "OpenCode minimal setup complete. Use 'opencode-minimal' preset."))

;;;###autoload
(defun opencode-setup-coding ()
  "Set up opencode optimized for coding tasks."
  (interactive)
  (opencode-register-coding-tools)
  (opencode-register-agents)
  (message "OpenCode coding setup complete. Use 'opencode-coding' preset."))

;;;###autoload
(defun opencode-setup-custom ()
  "Set up opencode with custom tool selection based on `opencode-enabled-tools'."
  (interactive)
  (cond
   ((eq opencode-enabled-tools 'all)
    (opencode-register-tools))
   ((eq opencode-enabled-tools 'essential)
   (opencode-register-essential-tools))
  ((eq opencode-enabled-tools 'coding)
   (opencode-register-coding-tools))
  ((eq opencode-enabled-tools 'minimal)
   (opencode-register-minimal-tools))
  ((listp opencode-enabled-tools)
   (opencode-register-selected-tools opencode-enabled-tools)))
  (opencode-register-agents)
  (message "OpenCode custom setup complete with %s tools."
           (if (listp opencode-enabled-tools)
               (format "%d" (length opencode-enabled-tools))
             (symbol-name opencode-enabled-tools))))

;;;###autoload
(defun opencode-unload ()
  "Remove opencode tools from gptel."
  (interactive)
  (setq gptel-tools
        (cl-remove-if (lambda (tool)
                        (member (opencode--get-tool-prop tool :category)
                                '("opencode" "filesystem" "command" "task" "emacs" "web")))
                      gptel-tools))
  (message "OpenCode tools removed from gptel."))

(provide 'opencode)

;;; opencode.el ends here
