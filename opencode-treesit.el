;;; opencode-treesit.el --- Tree-sitter integration for opencode -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: opencode
;; Keywords: tools, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This module provides tree-sitter integration for opencode.el, offering
;; syntax analysis, error detection, and code structure parsing as an
;; alternative to LSP-based functionality.

;;; Code:

(require 'treesit)

(defgroup opencode-treesit nil
  "Tree-sitter integration for opencode."
  :group 'opencode
  :prefix "opencode-treesit-")

(defcustom opencode-treesit-enabled t
  "Whether to use tree-sitter for code analysis when available."
  :type 'boolean
  :group 'opencode-treesit)

(defcustom opencode-treesit-fallback-to-lsp t
  "Whether to fallback to LSP when tree-sitter is unavailable."
  :type 'boolean
  :group 'opencode-treesit)

(defvar opencode-treesit--language-map
  '((python-mode . python)
    (python-ts-mode . python)
    (js-mode . javascript)
    (js2-mode . javascript)
    (javascript-mode . javascript)
    (js-ts-mode . javascript)
    (typescript-mode . typescript)
    (typescript-ts-mode . typescript)
    (c-mode . c)
    (c-ts-mode . c)
    (c++-mode . cpp)
    (c++-ts-mode . cpp)
    (java-mode . java)
    (java-ts-mode . java)
    (rust-mode . rust)
    (rust-ts-mode . rust)
    (go-mode . go)
    (go-ts-mode . go)
    (ruby-mode . ruby)
    (ruby-ts-mode . ruby)
    (php-mode . php)
    (php-ts-mode . php)
    (sh-mode . bash)
    (bash-ts-mode . bash)
    (json-mode . json)
    (json-ts-mode . json)
    (yaml-mode . yaml)
    (yaml-ts-mode . yaml)
    (html-mode . html)
    (html-ts-mode . html)
    (css-mode . css)
    (css-ts-mode . css))
  "Mapping from major modes to tree-sitter language names.")

(defun opencode-treesit-available-p ()
  "Check if tree-sitter is available in this Emacs instance."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)))

(defun opencode-treesit-language-for-mode (&optional mode)
  "Get the tree-sitter language for MODE (or current major mode)."
  (let ((mode (or mode major-mode)))
    (cdr (assq mode opencode-treesit--language-map))))

(defun opencode-treesit-language-available-p (language)
  "Check if LANGUAGE grammar is available for tree-sitter."
  (and (opencode-treesit-available-p)
       language
       (treesit-language-available-p language)))

(defun opencode-treesit-can-parse-buffer (&optional buffer)
  "Check if we can parse BUFFER (or current buffer) with tree-sitter."
  (with-current-buffer (or buffer (current-buffer))
    (let ((language (opencode-treesit-language-for-mode)))
      (opencode-treesit-language-available-p language))))

(defun opencode-treesit-get-parser (&optional buffer language)
  "Get or create a tree-sitter parser for BUFFER with LANGUAGE."
  (with-current-buffer (or buffer (current-buffer))
    (let ((lang (or language (opencode-treesit-language-for-mode))))
      (when (opencode-treesit-language-available-p lang)
        (or (treesit-parser-create lang)
            (car (treesit-parser-list)))))))

(defun opencode-treesit-get-tree (&optional buffer)
  "Get the syntax tree for BUFFER (or current buffer)."
  (when-let ((parser (opencode-treesit-get-parser buffer)))
    (treesit-parser-root-node parser)))

(defun opencode-treesit-query (query &optional buffer)
  "Execute tree-sitter QUERY on BUFFER (or current buffer).
Returns a list of (node . capture-name) pairs."
  (when-let ((tree (opencode-treesit-get-tree buffer))
             (language (opencode-treesit-language-for-mode)))
    (treesit-query-capture tree query language)))

(defun opencode-treesit-get-error-nodes (&optional buffer)
  "Get all error nodes in BUFFER (or current buffer)."
  (when-let ((tree (opencode-treesit-get-tree buffer)))
    (let ((errors '()))
      (treesit-search-subtree
       tree
       (lambda (node)
         (when (treesit-node-check node 'has-error)
           (push node errors))
         nil)
       nil t)
      (nreverse errors))))

(defun opencode-treesit-node-at-point (&optional point buffer)
  "Get the tree-sitter node at POINT in BUFFER."
  (when-let ((tree (opencode-treesit-get-tree buffer)))
    (treesit-node-at (or point (point)) tree)))

(defun opencode-treesit-get-diagnostics (&optional file-or-buffer)
  "Get syntax errors as diagnostic information for FILE-OR-BUFFER."
  (let ((buffer (if (stringp file-or-buffer)
                    (or (find-buffer-visiting file-or-buffer)
                        (find-file-noselect file-or-buffer))
                  file-or-buffer)))
    (when-let ((errors (opencode-treesit-get-error-nodes buffer)))
      (with-current-buffer (or buffer (current-buffer))
        (mapcar
         (lambda (node)
           (let ((start (treesit-node-start node))
                 (end (treesit-node-end node)))
             (let ((line (line-number-at-pos start))
                   (column (save-excursion
                             (goto-char start)
                             (current-column)))
                   (end-line (line-number-at-pos end))
                   (end-column (save-excursion
                                 (goto-char end)
                                 (current-column))))
               `(:line ,line
                 :column ,column
                 :end-line ,end-line
                 :end-column ,end-column
                 :message "Syntax error"
                 :severity 'error
                 :source "tree-sitter")))
         errors)))))

(defvar opencode-treesit--symbol-queries
  '((python . "[(function_definition name: (identifier) @function)
               (class_definition name: (identifier) @class)
               (assignment left: (identifier) @variable)]")
    (javascript . "[(function_declaration name: (identifier) @function)
                    (class_declaration name: (identifier) @class)
                    (variable_declarator name: (identifier) @variable)]")
    (typescript . "[(function_declaration name: (identifier) @function)
                    (class_declaration name: (identifier) @class)
                    (variable_declarator name: (identifier) @variable)]")
    (c . "[(function_definition declarator: (function_declarator declarator: (identifier) @function))
          (declaration declarator: (identifier) @variable)]")
    (cpp . "[(function_definition declarator: (function_declarator declarator: (identifier) @function))
            (class_specifier name: (type_identifier) @class)
            (declaration declarator: (identifier) @variable)]")
    (java . "[(method_declaration name: (identifier) @function)
             (class_declaration name: (identifier) @class)
             (variable_declarator name: (identifier) @variable)]")
    (rust . "[(function_item name: (identifier) @function)
             (struct_item name: (type_identifier) @struct)
             (let_declaration pattern: (identifier) @variable)]")
    (go . "[(function_declaration name: (identifier) @function)
           (type_declaration (type_spec name: (type_identifier) @type))
           (var_declaration (var_spec name: (identifier) @variable))]"))
  "Tree-sitter queries for extracting symbols by language.")

(defun opencode-treesit-get-symbols (&optional buffer)
  "Extract symbols (functions, classes, variables) from BUFFER."
  (when-let ((language (opencode-treesit-language-for-mode))
             (query (cdr (assq language opencode-treesit--symbol-queries))))
    (let ((captures (opencode-treesit-query query buffer)))
      (mapcar
       (lambda (capture)
         (let ((node (car capture))
               (type (cdr capture)))
           `(:name ,(treesit-node-text node)
             :type ,type
             :line ,(line-number-at-pos (treesit-node-start node))
             :column ,(save-excursion
                        (goto-char (treesit-node-start node))
                        (current-column))
             :start ,(treesit-node-start node)
             :end ,(treesit-node-end node))))
       captures))))

(defun opencode-treesit-enabled-and-available-p (&optional buffer)
  "Check if tree-sitter is enabled and available for BUFFER."
  (and opencode-treesit-enabled
       (opencode-treesit-can-parse-buffer buffer)))

(provide 'opencode-treesit)

;;; opencode-treesit.el ends here
