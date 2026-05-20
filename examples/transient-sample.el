;;; transient-sample.el --- Sample transient menu implementation  -*- lexical-binding: t; -*-

;;; Commentary:
;; This file demonstrates how to build a transient menu using the
;; built-in `transient' package (available in Emacs 28+).
;;
;; Usage:
;;   M-x sample-dispatch RET
;;
;; Or bind it to a key:
;;   (global-set-key (kbd "C-c t") #'sample-dispatch)

;;; Code:

(require 'transient)

;; -------------------------------------------------------------------------
;; 1. Define infix arguments (switches and options)
;; -------------------------------------------------------------------------

(transient-define-argument sample-verbosity-flag ()
  "Toggle verbose output."
  :description "Verbose"
  :class 'transient-switch
  :shortarg "-v"
  :argument "--verbose")

(transient-define-argument sample-output-option ()
  "Specify output file."
  :description "Output file"
  :class 'transient-option
  :shortarg "-o"
  :argument "--output="
  :reader #'transient-read-file)

(transient-define-argument sample-format-choice ()
  "Select output format."
  :description "Format"
  :class 'transient-option
  :shortarg "-f"
  :argument "--format="
  :choices '("json" "yaml" "org"))

;; -------------------------------------------------------------------------
;; 2. Define suffix commands (the actions)
;; -------------------------------------------------------------------------

(defun sample-process-files (files)
  "Process FILES according to current transient arguments."
  (interactive (list (transient-args transient-current-command)))
  (let ((verbose (transient-arg-value "--verbose" files))
        (output (transient-arg-value "--output=" files))
        (format (transient-arg-value "--format=" files)))
    (message "Processing files with: verbose=%s, output=%s, format=%s"
             verbose output format)))

(defun sample-show-buffer-info ()
  "Show information about the current buffer."
  (interactive)
  (message "Buffer: %s | Mode: %s | File: %s"
           (buffer-name)
           major-mode
           (or buffer-file-name "<no file>")))

(defun sample-search-project ()
  "Search the project using current settings."
  (interactive)
  (message "Searching project... (placeholder)"))

;; -------------------------------------------------------------------------
;; 3. Define the transient prefix (the menu itself)
;; -------------------------------------------------------------------------

;;;###autoload
(transient-define-prefix sample-dispatch ()
  "A sample transient menu demonstrating switches, options, and commands."
  :value '("--format=json")

  [["Options"
    ("-v" "Verbose" "--verbose")
    ("-o" "Output file" "--output=" :reader transient-read-file)
    (sample-format-choice)]

   ["Actions"
    ("p" "Process" sample-process-files)
    ("b" "Buffer info" sample-show-buffer-info)
    ("s" "Search project" sample-search-project)]

   ["Navigation"
    ("q" "Quit" transient-quit-one)]])

;; -------------------------------------------------------------------------
;; 4. Bonus: Nested transient example
;; -------------------------------------------------------------------------

;;;###autoload
(transient-define-prefix sample-sub-menu ()
  "A sub-menu that can be called from another transient."
  [["Sub-options"
    ("-c" "Count" "--count=" :reader transient-read-number)]
   ["Sub-actions"
    ("r" "Run" sample-process-files)
    ("b" "Back" transient-quit-one)]])

;; Add a command that opens the sub-menu from the main menu
(transient-define-prefix sample-dispatch-with-submenu ()
  "Main menu with a nested submenu."
  [["Main"
    ("-v" "Verbose" "--verbose")]
   ["Go to"
    ("m" "Sub-menu" sample-sub-menu)
    ("q" "Quit" transient-quit-one)]])

;; -------------------------------------------------------------------------
;; 5. Setup / Keybinding (optional)
;; -------------------------------------------------------------------------

;; Uncomment to bind:
;; (global-set-key (kbd "C-c t") #'sample-dispatch)

(provide 'transient-sample)
;;; transient-sample.el ends here
