;;; gptel-commit-message.el --- Generate git commit messages

;; Copyright (C) 2026 Nicolai Singh

;; Author: Nicolai Singh <nicolaisingh at pm.me>
;; Created 22 Mar 2026
;; Version: 1.0
;; Keywords: gptel
;; Homepage: https://github.com/nicolaisingh/dotemacs

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'gptel)
(require 'magit)

(defcustom gptel-commit-message-exclude-files
  '("*.eld" "package-lock.json")
  "File globs to exclude from diff generation.")

;;;###autoload
(defun gptel-commit-message ()
  "Ask gptel to write a git commit message."
  (interactive)
  (let* ((all-files (split-string (magit-git-output "diff" "--cached" "--name-only") "\n" t))
         (excluded-regexps (mapcar #'wildcard-to-regexp gptel-commit-message-exclude-files))
         (included-files (seq-filter
                          (lambda (file)
                            (not (seq-some (lambda (re) (string-match-p re file))
                                           excluded-regexps)))
                          all-files))
         (staged-diff (if included-files
                          (apply #'magit-git-output "diff" "--cached" "--" included-files)
                        "")))

    (if (zerop (length staged-diff))
        (message "gptel-commit-message: Nothing to diff and analyze")
      (message "gptel-commit-message: Analyzing diff for the ff. files: %s" included-files)
      (gptel-with-preset 'git-commit
        (gptel-request staged-diff
          :in-place t
          :stream t
          :context nil
          :callback (lambda (response info)
                      (when (stringp response)
                        (with-current-buffer (plist-get info :buffer)
                          (insert response)))))))))

(provide ' gptel-commit-message)
;;; gptel-commit-message.el ends here
