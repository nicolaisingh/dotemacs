;;; adoc-addons.el --- Additional functionality for adoc-mode.

;; Copyright (C) 2026 Nicolai Singh

;; Author: Nicolai Singh <nicolaisingh at pm.me>
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'adoc-mode)

;;;###autoload
(defun adoc-addons-compile-to-pdf ()
  "Compile the current AsciiDoc buffer to PDF using asciidoctor-pdf."
  (interactive)
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Buffer is not visiting a file"))
    (compile (concat "asciidoctor-pdf " (shell-quote-argument file)))))

;;;###autoload
(defun adoc-addons-view-output ()
  "Open the output file (PDF or HTML) for the current AsciiDoc buffer."
  (interactive)
  (let* ((file (buffer-file-name))
         (base (and file (file-name-sans-extension file)))
         (pdf (concat base ".pdf"))
         (html (concat base ".html"))
         (htm (concat base ".htm")))
    (unless file
      (user-error "Buffer is not visiting a file"))
    (cond ((file-exists-p pdf) (find-file pdf))
          ((file-exists-p html) (find-file html))
          ((file-exists-p htm) (find-file htm))
          (t (user-error "No PDF or HTML output found for %s" file)))))

(defun adoc-addons--promote-demote-lines (fn &optional beg end)
  "Apply FN to each relevant line from BEG to END.
If BEG and END are nil, use the active region or current line."
  (let ((beg (or beg (if (use-region-p) (region-beginning) (line-beginning-position))))
        (end (or end (if (use-region-p) (region-end) (line-end-position)))))
    (save-excursion
      (goto-char beg)
      (beginning-of-line)
      (while (and (< (point) end) (not (eobp)))
        (let ((size-before (buffer-size)))
          (funcall fn)
          (setq end (+ end (- (buffer-size) size-before))))
        (forward-line 1)))))

(defun adoc-addons--promote ()
  "Promote the title or list item at the current line.
Return non-nil if a change was made."
  (cond
   ((looking-at "=\\{2,\\} ") (delete-char 1) t)
   ((looking-at "\\*\\{2,\\} ") (delete-char 1) t)
   ((looking-at "\\.\\{2,\\} ") (delete-char 1) t)
   (t nil)))

(defun adoc-addons--demote ()
  "Demote the title or list item at the current line.
Return non-nil if a change was made."
  (cond
   ((looking-at "=+ ") (insert ?=) t)
   ((looking-at "\\*+ ") (insert ?*) t)
   ((looking-at "\\.+ ") (insert ?.) t)
   (t nil)))

(defun adoc-addons--promote-title ()
  "Promote the section title at the current line.
Return non-nil if a change was made."
  (when (looking-at "=\\{2,\\} ")
    (delete-char 1)
    t))

(defun adoc-addons--demote-title ()
  "Demote the section title at the current line.
Return non-nil if a change was made."
  (when (looking-at "=+ ")
    (insert ?=)
    t))

(defun adoc-addons--prefix (&optional type)
  "Return the structural prefix of the current line, or nil.
The return value is a cons cell (MARKER . LEVEL) where MARKER is
the matched string and LEVEL is its length.

TYPE can be `list' to match only list-item markers (*, ., -),
`title' to match only title markers (=), or nil to match any."
  (save-excursion
    (beginning-of-line)
    (cond
     ((and (memq type '(nil title))
           (looking-at "^\\(=+\\) "))
      (cons (match-string-no-properties 1) (length (match-string 1))))
     ((and (memq type '(nil list))
           (looking-at "^\\(\\*+\\) "))
      (cons (match-string-no-properties 1) (length (match-string 1))))
     ((and (memq type '(nil list))
           (looking-at "^\\(\\.+\\) "))
      (cons (match-string-no-properties 1) (length (match-string 1))))
     ((and (memq type '(nil list))
           (looking-at "^\\(-\\) "))
      (cons (match-string-no-properties 1) 1))
     (t nil))))

(defun adoc-addons--bounds (&optional type)
  "Return (START . END) of the structural element at point.
TYPE can be `list' to match only list items, `title' to match
only section titles, or nil to match either."
  (save-excursion
    (beginning-of-line)
    ;; Search backward if not on the requested type
    (when (if type
              (not (adoc-addons--prefix type))
            (not (adoc-addons--prefix)))
      (while (and (not (bobp))
                  (progn (forward-line -1)
                         (and (not (looking-at "^[ \t]*$"))
                              (if type
                                  (not (adoc-addons--prefix type))
                                (not (adoc-addons--prefix)))))))))
  (let* ((prefix (adoc-addons--prefix type))
         (actual-type
          (cond
           ((and (memq type '(nil title))
                 (looking-at "^=+ "))
            'title)
           ((and (memq type '(nil list))
                 (looking-at "^\\(\\*+\\|\\.+\\|-\\) "))
            'list))))
    (unless prefix
      (user-error "Not on a list item or section title"))
    (let* ((level (cdr prefix))
           (start (line-beginning-position))
           (end (if (eq actual-type 'list)
                    (save-excursion
                      (forward-line 1)
                      (let ((stop nil))
                        (while (and (not (eobp)) (not stop))
                          (let ((next-prefix (adoc-addons--prefix 'list)))
                            (cond
                             ;; Next sibling or parent marker -> stop
                             ((and next-prefix (<= (cdr next-prefix) level))
                              (setq stop (point)))
                             ;; Blank line not followed by nested item -> stop
                             ((and (not next-prefix)
                                   (looking-at "^[ \t]*$"))
                              (let ((blank-line (line-beginning-position)))
                                (forward-line 1)
                                (while (and (not (eobp)) (looking-at "^[ \t]*$"))
                                  (forward-line 1))
                                (if (and (not (eobp))
                                         (adoc-addons--prefix 'list)
                                         (> (cdr (adoc-addons--prefix 'list)) level))
                                    ;; Nested item follows; continue
                                    (forward-line 1)
                                  (setq stop blank-line))))
                             ;; Anything else -> part of current item, keep going
                             (t (forward-line 1)))))
                        (if stop stop (point))))
                  (save-excursion
                    (forward-line 1)
                    (if (re-search-forward (format "^=\\{1,%d\\} " level) nil t)
                        (line-beginning-position)
                      (point-max))))))
      (cons start end))))

;;;###autoload
(defun adoc-addons-insert-list-item ()
  "Insert a new list item at the same level as the current one.
If point is on a list item, insert a new line below with the same
marker.  If point is in the middle of the item text, split the
text at point."
  (interactive)
  (let ((prefix (adoc-addons--prefix 'list)))
    (if prefix
        (let* ((marker (car prefix))
               (marker-end-pos (+ (line-beginning-position)
                                  (current-indentation)
                                  (length marker)
                                  1)))
          (when (< (point) marker-end-pos)
            (end-of-line))
          (newline)
          (insert marker " "))
      (newline))))

;;;###autoload
(defun adoc-addons-move-item-up ()
  "Move the current list item or section (and its subtree) up."
  (interactive)
  (let* ((is-section (save-excursion (beginning-of-line) (adoc-addons--prefix 'title)))
         (current-bounds (if is-section
                             (adoc-addons--bounds 'title)
                           (adoc-addons--bounds 'list)))
         (current-start (car current-bounds))
         (current-end (cdr current-bounds))
         (current-level (if is-section
                            (save-excursion
                              (goto-char current-start)
                              (cdr (adoc-addons--prefix 'title)))
                          (save-excursion
                            (goto-char current-start)
                            (cdr (adoc-addons--prefix 'list)))))
         (current-text (buffer-substring-no-properties current-start current-end)))
    (if is-section
        (let ((prev-start (save-excursion
                            (goto-char current-start)
                            (forward-line -1)
                            (when (re-search-backward (format "^=\\{%d\\} " current-level) nil t)
                              (line-beginning-position)))))
          (unless prev-start
            (user-error "No previous section at this level"))
          (let* ((prev-bounds (save-excursion
                                (goto-char prev-start)
                                (adoc-addons--bounds 'title)))
                 (prev-start (car prev-bounds))
                 (prev-end (cdr prev-bounds))
                 (prev-text (buffer-substring-no-properties prev-start prev-end)))
            (delete-region current-start current-end)
            (goto-char current-start)
            (insert prev-text)
            (delete-region prev-start prev-end)
            (goto-char prev-start)
            (insert current-text)
            (goto-char prev-start)))
      (save-excursion
        (goto-char current-start)
        (forward-line -1)
        (let ((prefix (adoc-addons--prefix 'list)))
          (while (and (not (bobp))
                      (or (not prefix)
                          (> (cdr prefix) current-level)))
            (forward-line -1)
            (setq prefix (adoc-addons--prefix 'list)))
          (unless (and prefix (= (cdr prefix) current-level))
            (user-error "No previous list item at this level"))
          (let* ((prev-bounds (adoc-addons--bounds 'list))
                 (prev-start (car prev-bounds))
                 (prev-end (cdr prev-bounds))
                 (prev-text (buffer-substring-no-properties prev-start prev-end)))
            ;; Don't cross blank lines or non-list text between lists
            (save-excursion
              (goto-char prev-end)
              (while (< (point) current-start)
                (unless (adoc-addons--prefix 'list)
                  (user-error "No previous list item at this level"))
                (forward-line 1)))
            (delete-region current-start current-end)
            (goto-char current-start)
            (insert prev-text)
            (delete-region prev-start prev-end)
            (goto-char prev-start)
            (insert current-text)
            (goto-char prev-start)))))))

;;;###autoload
(defun adoc-addons-move-item-down ()
  "Move the current list item or section (and its subtree) down."
  (interactive)
  (let* ((is-section (save-excursion (beginning-of-line) (adoc-addons--prefix 'title)))
         (current-bounds (if is-section
                             (adoc-addons--bounds 'title)
                           (adoc-addons--bounds 'list)))
         (current-start (car current-bounds))
         (current-end (cdr current-bounds))
         (current-level (if is-section
                            (save-excursion
                              (goto-char current-start)
                              (cdr (adoc-addons--prefix 'title)))
                          (save-excursion
                            (goto-char current-start)
                            (cdr (adoc-addons--prefix 'list)))))
         (current-text (buffer-substring-no-properties current-start current-end)))
    (if is-section
        (let ((final-pos nil))
          (save-excursion
            (goto-char current-end)
            (when (= (point) (point-max))
              (user-error "No next section at this level"))
            (let ((next-start (when (re-search-forward (format "^=\\{%d\\} " current-level) nil t)
                                (line-beginning-position))))
              (unless next-start
                (user-error "No next section at this level"))
              (let* ((next-bounds (save-excursion
                                    (goto-char next-start)
                                    (adoc-addons--bounds 'title)))
                     (next-start (car next-bounds))
                     (next-end (cdr next-bounds))
                     (next-text (buffer-substring-no-properties next-start next-end)))
                (delete-region next-start next-end)
                (goto-char next-start)
                (insert current-text)
                (delete-region current-start current-end)
                (goto-char current-start)
                (insert next-text)
                (setq final-pos (+ next-start (- (length next-text) (length current-text)))))))
          (when final-pos
            (goto-char final-pos)))
      (let ((final-pos nil))
        (save-excursion
          (goto-char current-end)
          (when (= (point) (point-max))
            (user-error "No next list item at this level"))
          (let ((prefix (adoc-addons--prefix 'list)))
            (while (and (not (eobp))
                        (or (not prefix)
                            (> (cdr prefix) current-level)))
              (forward-line 1)
              (setq prefix (adoc-addons--prefix 'list)))
            (unless (and prefix (= (cdr prefix) current-level))
              (user-error "No next list item at this level"))
            (let* ((next-bounds (adoc-addons--bounds 'list))
                   (next-start (car next-bounds))
                   (next-end (cdr next-bounds))
                   (next-text (buffer-substring-no-properties next-start next-end)))
              ;; Don't cross blank lines or non-list text between lists
              (save-excursion
                (goto-char current-end)
                (while (< (point) next-start)
                  (unless (adoc-addons--prefix 'list)
                    (user-error "No next list item at this level"))
                  (forward-line 1)))
              (delete-region next-start next-end)
              (goto-char next-start)
              (insert current-text)
              (delete-region current-start current-end)
              (goto-char current-start)
              (insert next-text)
              (setq final-pos (+ next-start (- (length next-text) (length current-text)))))))
        (when final-pos
          (goto-char final-pos))))))

;;;###autoload
(defun adoc-addons-line-to-title (level)
  "Toggle and convert the current line to an AsciiDoc section title.

With a numeric prefix arg LEVEL, use that title level:
  C-0 -> =, C-1 -> ==, C-2 -> ===, etc.
Without a prefix arg, default to the level of the preceding section title.
If no preceding section is found, default to level 0 (=)."
  (interactive "P")
  (save-excursion
    (beginning-of-line)
    ;; Remove any existing leading = signs and whitespace
    (if (looking-at "=+[ \t]*")
        (replace-match "")
      (progn
        ;; Trim trailing whitespace
        (end-of-line)
        (delete-horizontal-space)
        (beginning-of-line)
        ;; Determine level
        (let* ((level (if level
                          (prefix-numeric-value level)
                        (or (save-excursion
                              (beginning-of-line)
                              (unless (bobp)
                                (backward-char))
                              (when (re-search-backward "^\\(=+\\)[ \t]" nil t)
                                (1- (length (match-string 1)))))
                            0)))
               (prefix (make-string (1+ level) ?=)))
          (insert prefix " "))))))

;;;###autoload
(defun adoc-addons-promote ()
  "Promote AsciiDoc title or list item at point.
If the region is active, promote all items in the region."
  (interactive)
  (adoc-addons--promote-demote-lines #'adoc-addons--promote))

;;;###autoload
(defun adoc-addons-demote ()
  "Demote AsciiDoc title or list item at point.
If the region is active, demote all items in the region."
  (interactive)
  (adoc-addons--promote-demote-lines #'adoc-addons--demote))

;;;###autoload
(defun adoc-addons-promote-item-and-children ()
  "Promote the current list item or section and all its children."
  (interactive)
  (let* ((bounds (condition-case nil
                     (adoc-addons--bounds)
                   (error nil))))
    (unless bounds
      (user-error "Not on a list item or section title"))
    (let ((type (save-excursion
                  (goto-char (car bounds))
                  (cond
                   ((adoc-addons--prefix 'title) 'title)
                   ((adoc-addons--prefix 'list) 'list)))))
      (if (eq type 'list)
          (progn
            ;; Guard: don't promote children if the current item can't be promoted
            (let ((prefix (save-excursion
                            (goto-char (car bounds))
                            (adoc-addons--prefix 'list))))
              (unless (and prefix (> (cdr prefix) 1))
                (user-error "Cannot promote item further")))
            ;; List items: promote titles, bullets, and numbered items
            (adoc-addons--promote-demote-lines
             #'adoc-addons--promote
             (car bounds) (cdr bounds)))
        ;; Sections: only promote titles
        (save-excursion
          (goto-char (car bounds))
          (beginning-of-line)
          ;; Guard: don't promote children if the current title can't be promoted
          (let ((prefix (adoc-addons--prefix 'title)))
            (unless (and prefix (> (cdr prefix) 1))
              (user-error "Cannot promote section further")))
          (adoc-addons--promote-demote-lines
           #'adoc-addons--promote-title
           (point) (cdr bounds)))))))

;;;###autoload
(defun adoc-addons-demote-item-and-children ()
  "Demote the current list item or section and all its children."
  (interactive)
  (let* ((bounds (condition-case nil
                     (adoc-addons--bounds)
                   (error nil))))
    (unless bounds
      (user-error "Not on a list item or section title"))
    (let ((type (save-excursion
                  (goto-char (car bounds))
                  (cond
                   ((adoc-addons--prefix 'title) 'title)
                   ((adoc-addons--prefix 'list) 'list)))))
      (if (eq type 'list)
          ;; List items: demote titles, bullets, and numbered items
          (adoc-addons--promote-demote-lines
           #'adoc-addons--demote
           (car bounds) (cdr bounds))
        ;; Sections: only demote titles
        (save-excursion
          (goto-char (car bounds))
          (adoc-addons--promote-demote-lines
           #'adoc-addons--demote-title
           (point) (cdr bounds)))))))

(provide 'adoc-addons)
;;; adoc-addons.el ends here
