;;; opencode-lsp.el --- LSP integration for opencode tools -*- lexical-binding: t; -*-

;;; Commentary:

;; This file provides LSP integration for opencode tools, similar to the
;; original opencode's LSP functionality. It integrates with lsp-mode
;; to provide diagnostics, symbol information, and other LSP features.

;;; Code:

(require 'lsp-mode nil t)

;; Customization
(defcustom opencode-lsp-enabled t
  "Whether to enable LSP integration in opencode tools."
  :type 'boolean
  :group 'opencode)

(defcustom opencode-lsp-show-diagnostics t
  "Whether to show LSP diagnostics in tool output."
  :type 'boolean
  :group 'opencode)

(defcustom opencode-lsp-auto-start t
  "Whether to automatically start LSP when touching files."
  :type 'boolean
  :group 'opencode)

;; LSP Helper Functions

(defun opencode-lsp-available-p ()
  "Check if LSP is available and enabled."
  (and opencode-lsp-enabled
       (featurep 'lsp-mode)))

(defun opencode-lsp-touch-file (filepath &optional ensure-lsp)
  "Touch a file to warm up LSP client.
If ENSURE-LSP is non-nil, ensure LSP is started for the file."
  (when (opencode-lsp-available-p)
    (let ((expanded-path (expand-file-name filepath)))
      (when (file-exists-p expanded-path)
        (with-temp-buffer
          (insert-file-contents expanded-path)
          (set-visited-file-name expanded-path)
          (when (and ensure-lsp opencode-lsp-auto-start)
            (condition-case nil
                (lsp-deferred)
              (error nil))))))))

(defun opencode-lsp-get-diagnostics (&optional filepath)
  "Get LSP diagnostics for FILEPATH or all open files."
  (when (opencode-lsp-available-p)
    (if filepath
        (opencode-lsp-get-file-diagnostics filepath)
      (opencode-lsp-get-all-diagnostics))))

(defun opencode-lsp-get-file-diagnostics (filepath)
  "Get LSP diagnostics for a specific file."
  (let ((expanded-path (expand-file-name filepath))
        (diagnostics '()))
    (when (file-exists-p expanded-path)
      (with-temp-buffer
        (insert-file-contents expanded-path)
        (set-visited-file-name expanded-path)
        (when (lsp-workspaces)
          (setq diagnostics (lsp-diagnostics-stats-for expanded-path)))))
    diagnostics))

(defun opencode-lsp-get-all-diagnostics ()
  "Get LSP diagnostics for all files in workspace."
  (when (and (opencode-lsp-available-p) (lsp-workspaces))
    (let ((all-diagnostics '()))
      (maphash (lambda (file-uri diagnostics)
                 (when diagnostics
                   (push (cons (lsp--uri-to-path file-uri) diagnostics) all-diagnostics)))
               (lsp-diagnostics))
      all-diagnostics)))

(defun opencode-lsp-format-diagnostics (diagnostics &optional filepath)
  "Format LSP diagnostics for display."
  (if (not diagnostics)
      ""
    (let ((formatted ""))
      (if filepath
          ;; Single file diagnostics
          (when diagnostics
            (setq formatted (format "\n<file_diagnostics file=\"%s\">\n" filepath))
            (dolist (diag diagnostics)
              (setq formatted 
                    (concat formatted
                            (format "%s:%d:%d: %s: %s\n"
                                    (file-name-nondirectory filepath)
                                    (1+ (lsp-diagnostic-line diag))
                                    (1+ (lsp-diagnostic-character diag))
                                    (lsp-diagnostic-severity diag)
                                    (lsp-diagnostic-message diag)))))
            (setq formatted (concat formatted "</file_diagnostics>\n")))
        ;; All diagnostics
        (dolist (file-diags diagnostics)
          (let ((file (car file-diags))
                (diags (cdr file-diags)))
            (when diags
              (setq formatted 
                    (concat formatted
                            (format "\n<project_diagnostics file=\"%s\">\n" file)))
              (dolist (diag diags)
                (setq formatted 
                      (concat formatted
                              (format "%s:%d:%d: %s: %s\n"
                                      (file-name-nondirectory file)
                                      (1+ (lsp-diagnostic-line diag))
                                      (1+ (lsp-diagnostic-character diag))
                                      (lsp-diagnostic-severity diag)
                                      (lsp-diagnostic-message diag)))))
              (setq formatted (concat formatted "</project_diagnostics>\n"))))))
      formatted)))

(defun opencode-lsp-workspace-symbols (query)
  "Search for workspace symbols matching QUERY."
  (when (and (opencode-lsp-available-p) (lsp-workspaces))
    (condition-case nil
        (lsp-request "workspace/symbol" `(:query ,query))
      (error nil))))

(defun opencode-lsp-document-symbols (filepath)
  "Get document symbols for FILEPATH."
  (when (opencode-lsp-available-p)
    (let ((expanded-path (expand-file-name filepath)))
      (when (file-exists-p expanded-path)
        (with-temp-buffer
          (insert-file-contents expanded-path)
          (set-visited-file-name expanded-path)
          (when (lsp-workspaces)
            (condition-case nil
                (lsp-request "textDocument/documentSymbol"
                           (lsp--text-document-identifier))
              (error nil))))))))

(defun opencode-lsp-hover (filepath line character)
  "Get hover information for position in file."
  (when (opencode-lsp-available-p)
    (let ((expanded-path (expand-file-name filepath)))
      (when (file-exists-p expanded-path)
        (with-temp-buffer
          (insert-file-contents expanded-path)
          (set-visited-file-name expanded-path)
          (when (lsp-workspaces)
            (condition-case nil
                (lsp-request "textDocument/hover"
                           (lsp--text-document-position-params line character))
              (error nil))))))))

;; Integration with opencode tools

(defun opencode-lsp-enhance-write-output (filepath output)
  "Enhance write tool output with LSP diagnostics."
  (if (not (and opencode-lsp-show-diagnostics (opencode-lsp-available-p)))
      output
    (let ((diagnostics (opencode-lsp-get-file-diagnostics filepath)))
      (if diagnostics
          (concat output (opencode-lsp-format-diagnostics diagnostics filepath))
        output))))

(defun opencode-lsp-enhance-edit-output (filepath output)
  "Enhance edit tool output with LSP diagnostics."
  (if (not (and opencode-lsp-show-diagnostics (opencode-lsp-available-p)))
      output
    (let ((file-diagnostics (opencode-lsp-get-file-diagnostics filepath))
          (all-diagnostics (opencode-lsp-get-all-diagnostics)))
      (concat output
              (when file-diagnostics
                (opencode-lsp-format-diagnostics file-diagnostics filepath))
              (when all-diagnostics
                (opencode-lsp-format-diagnostics all-diagnostics))))))

;; New LSP-specific tools

(defun opencode-lsp-diagnostics (filepath)
  "Get LSP diagnostics for a file."
  (if (not (opencode-lsp-available-p))
      "LSP not available or disabled"
    (opencode-lsp-touch-file filepath t)
    (let ((diagnostics (opencode-lsp-get-file-diagnostics filepath)))
      (if diagnostics
          (opencode-lsp-format-diagnostics diagnostics filepath)
        "No errors found"))))

(defun opencode-lsp-symbols (query)
  "Search workspace symbols."
  (if (not (opencode-lsp-available-p))
      "LSP not available or disabled"
    (let ((symbols (opencode-lsp-workspace-symbols query)))
      (if symbols
          (mapconcat (lambda (symbol)
                       (format "%s (%s) - %s"
                               (lsp-get symbol :name)
                               (lsp-get symbol :kind)
                               (lsp-get symbol :location :uri)))
                     symbols "\n")
        "No symbols found"))))

(provide 'opencode-lsp)

;;; opencode-lsp.el ends here