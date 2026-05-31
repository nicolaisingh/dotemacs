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
                         (or (looking-at "^[ \t]*$")
                             (if type
                                 (not (adoc-addons--prefix type))
                               (not (adoc-addons--prefix))))))))
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
             (end (cond
                   ((eq actual-type 'list)
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
                        (if stop stop (point)))))
                   ((eq actual-type 'title)
                    (save-excursion
                      (forward-line 1)
                      (if (re-search-forward (format "^=\\{1,%d\\} " level) nil t)
                          (line-beginning-position)
                        (point-max)))))))
        (cons start end)))))

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
(defun adoc-addons-insert-section-after ()
  "Insert a new section title at the same level after the current section.
The new section is inserted after the current section and all its
children, but before any following sibling sections."
  (interactive)
  (let* ((bounds (adoc-addons--bounds 'title))
         (end (cdr bounds))
         (has-next-sibling (< end (point-max)))
         (level (save-excursion
                  (goto-char (car bounds))
                  (cdr (adoc-addons--prefix 'title))))
         (prefix (make-string level ?=)))
    (goto-char end)
    (unless (bolp)
      (insert "\n"))
    (insert prefix " \n")
    (forward-line -1)
    (beginning-of-line)
    (unless (save-excursion
              (forward-line -1)
              (looking-at "^[ \t]*$"))
      (insert "\n"))
    (end-of-line)
    (when has-next-sibling
      (save-excursion
        (forward-line 1)
        (unless (looking-at "^[ \t]*$")
          (insert "\n"))))))

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

;;;###autoload
(defun adoc-addons-toggle-checkbox ()
  "Toggle the checkbox on the current list item between [ ] and [x].
Return non-nil if a checkbox was toggled."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\([*.-]+\\|[0-9]+\\.\\) ")
      (cond ((re-search-forward "\\[ \\]" (line-end-position) t)
             (replace-match "[x]" t t)
             t)
            ((re-search-forward "\\[x\\]" (line-end-position) t)
             (replace-match "[ ]" t t)
             t)
            (t nil)))))

;;;###autoload
(defun adoc-addons-dwim ()
  "Do What I Mean in adoc-mode.
If point is on a checklist item, toggle its checkbox.
Otherwise, do nothing."
  (interactive)
  (adoc-addons-toggle-checkbox))

;; -------------------------------------------------------------------
;; Tables
;; -------------------------------------------------------------------

(defvar adoc-addons--table-block-delimiter
  "^\\s-*[|,]====*\\s-*$"
  "Starting and ending delimiters for a table block.")

(defun adoc-addons--table-next-cell (sep-char start end)
  "Move to the next cell using SEP-CHAR.
START and END are the table boundaries."
  (let ((line-end (line-end-position))
        (sep-str (char-to-string sep-char)))
    (cond
     ;; Already on a delimiter — skip into the cell after it
     ((eq (char-after) sep-char)
      (forward-char 1)
      (skip-syntax-forward " "))

     ;; Delimiter exists later on this line
     ((search-forward sep-str line-end t)
      (skip-syntax-forward " "))

     ;; No more delimiters — next row, or new row at end of table
     (t
      (let ((next-line (save-excursion (forward-line 1) (point))))
        (if (>= next-line end)
            (adoc-addons--table-insert-row start end sep-char)
          (forward-line 1)
          (beginning-of-line)
          (skip-syntax-forward " ")
          ;; Skip blank/header-separator lines
          (while (and (< (point) end)
                      (or (looking-at-p "^\\s-*$")
                          (looking-at-p (concat "^\\s-*" sep-str "===\\s-*$"))))
            (forward-line 1)
            (beginning-of-line)
            (skip-syntax-forward " "))
          (when (and (< (point) end)
                     (eq (char-after) sep-char))
            (forward-char 1)
            (skip-syntax-forward " "))))))))

(defun adoc-addons--table-prev-cell (sep-char start)
  "Move to the previous cell using SEP-CHAR.
START is the table start position."
  (let ((line-start (line-beginning-position))
        (sep-str (char-to-string sep-char))
        (orig (point)))
    (cond
     ;; Sitting on a delimiter — step backward into the cell before it
     ((eq (char-after) sep-char)
      (backward-char 1)
      (skip-syntax-backward " ")
      (if (search-backward sep-str line-start t)
          (progn
            (forward-char 1)
            (skip-syntax-forward " "))
        ;; No earlier delimiter — check for a first cell without leading delimiter
        (let ((first-cell-start
               (save-excursion
                 (goto-char line-start)
                 (skip-syntax-forward " ")
                 (and (not (eq (char-after) sep-char))
                      (point)))))
          (if first-cell-start
              (goto-char first-cell-start)
            (goto-char orig)
            (adoc-addons--table-prev-row-last-cell sep-char start)))))

     ;; Inside a cell — find its leading delimiter, then the one before it
     ((progn
        (skip-syntax-backward " ")
        (search-backward sep-str line-start t))
      ;; Point is now at the delimiter that begins the current cell.
      (let ((delim-pos (point)))
        (skip-syntax-backward " ")
        (if (search-backward sep-str line-start t)
            ;; Found the delimiter before the previous cell
            (progn
              (forward-char 1)
              (skip-syntax-forward " "))
          ;; No earlier delimiter — check for a first cell without leading delimiter
          (let ((first-cell-start
                 (save-excursion
                   (goto-char line-start)
                   (skip-syntax-forward " ")
                   (and (< (point) delim-pos)
                        (not (eq (char-after) sep-char))
                        (point)))))
            (if first-cell-start
                (goto-char first-cell-start)
              ;; We're in the first cell — previous row
              (goto-char orig)
              (adoc-addons--table-prev-row-last-cell sep-char start))))))

     ;; No delimiter on this line
     (t
      (goto-char orig)
      (adoc-addons--table-prev-row-last-cell sep-char start)))))

(defun adoc-addons--table-prev-row-last-cell (sep-char start)
  "Move point to the last cell of the row above.
If already in the first row, do nothing."
  (let ((sep-str (char-to-string sep-char))
        (orig (point)))
    (forward-line -1)
    (end-of-line)
    ;; Skip blank lines and delimiter lines
    (while (and (not (bobp))
                (>= (point) start)
                (save-excursion
                  (beginning-of-line)
                  (or (looking-at-p "^\\s-*$")
                      (looking-at-p (concat "^\\s-*" sep-str "===\\s-*$")))))
      (forward-line -1)
      (end-of-line))
    ;; If we ended up on or before the opening delimiter, restore point
    (if (< (point) start)
        (goto-char orig)
      ;; Find the last separator on this line
      (let ((last-sep nil))
        (save-excursion
          (beginning-of-line)
          (while (search-forward sep-str (line-end-position) t)
            (setq last-sep (point))))
        (if last-sep
            (progn
              (goto-char last-sep)
              (skip-syntax-forward " "))
          ;; No separator — first cell without leading delimiter
          (beginning-of-line)
          (skip-syntax-forward " "))))))

(defun adoc-addons--table-insert-row (start end sep-char)
  "Insert a blank row at the end of the table."
  (goto-char end)
  (beginning-of-line)
  (let* ((indent (adoc-addons--table-indent start))
         (sep-str (char-to-string sep-char))
         (col-count (adoc-addons--table-column-count
                     start end (regexp-quote sep-str))))
    (insert "\n" indent)
    (if (string= sep-str "|")
        ;; Pipe style:  |  |  |
        (progn
          (insert sep-str " ")
          (dotimes (_ (1- col-count))
            (insert " " sep-str " ")))
      ;; Comma style:  ,  ,
      (progn
        (dotimes (_ (1- col-count))
          (insert sep-str " "))
        (insert " ")))))

(defun adoc-addons--table-bounds ()
  "Return (START . END) of the AsciiDoc table at point."
  ;; TODO: Handle csv style tables properly
  (let (start end start-pos)
    ;; Move one line up first.  If we are still in a table, then start
    ;; from here since point initially might be at the closing
    ;; delimiter.  Otherwise, if there is no table content, then we
    ;; might have moved out of it.
    (save-excursion
      (beginning-of-line)
      (forward-line -1)
      (while (and (not (bobp))
                  (looking-at-p "^$"))
        (forward-line -1))
      (when (or (looking-at-p adoc-addons--table-block-delimiter)
                (looking-at-p "^|"))
        (setq start-pos (point))))
    ;; Look for the start delimiter
    (save-excursion
      (when start-pos
        (goto-char start-pos))
      (beginning-of-line)
      (unless (looking-at-p adoc-addons--table-block-delimiter)
        (while (and (not (bobp))
                    (not (looking-at-p adoc-addons--table-block-delimiter))
                    (or (looking-at-p "^$")
                        (looking-at-p "^\\|")))
          (forward-line -1))
        (unless (looking-at-p adoc-addons--table-block-delimiter)
          (error "Not inside an AsciiDoc table")))
      (setq start (point)))
    ;; Look for the end delimiter
    (save-excursion
      (beginning-of-line)
      (unless start-pos
        ;; Only move one line forward if point is not in the ending
        ;; delimiter
        (forward-line 1))
      (while (and (not (eobp))
                  (not (looking-at-p adoc-addons--table-block-delimiter))
                  (or (looking-at-p "^$")
                      (looking-at-p "^\\|"))
                  (forward-line 1)))
      (unless (looking-at adoc-addons--table-block-delimiter)
        (error "Unclosed AsciiDoc table"))
      (goto-char (1- (match-end 0)))
      (setq end (point)))
    (cons start end)))

(defun adoc-addons--table-separator (pos)
  "Return | or , for the table at POS."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (skip-syntax-forward " ")
    (char-to-string (char-after))))

(defun adoc-addons--table-indent (pos)
  "Return leading whitespace of the line at POS."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (buffer-substring-no-properties
     (line-beginning-position)
     (progn (skip-syntax-forward " ")
            (point)))))

(defun adoc-addons--table-column-count (start end sep-re)
  "Count columns from the first data row."
  (save-excursion
    (goto-char start)
    (forward-line 1)
    (while (and (< (point) end)
                (or (looking-at-p adoc-addons--table-block-delimiter)
                    (looking-at-p "^\\s-*$")))
      (forward-line 1))
    (if (>= (point) end)
        1
      (length (split-string
               (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position))
               (concat "\\s-*" sep-re "\\s-*")
               t)))))

;;;###autoload
(defun adoc-addons-table-tab ()
  "Move to the next cell in an AsciiDoc table.
If already in the last cell of the last row, insert a new blank
row and move to its first cell.  Falls back to
`indent-for-tab-command' when point is outside a table."
  (interactive)
  (let ((bounds (ignore-errors (adoc-addons--table-bounds))))
    (if (not bounds)
        (indent-for-tab-command)
      (let* ((start (car bounds))
             (end   (cdr bounds))
             (sep   (adoc-addons--table-separator start))
             (sep-char (string-to-char sep)))
        ;; If we're on a delimiter or blank separator line, step off it
        (when (save-excursion
                (beginning-of-line)
                (or (looking-at-p adoc-addons--table-block-delimiter)
                    (looking-at-p "^\\s-*$")))
          (forward-line 1)
          (beginning-of-line))
        ;; Normalise point when sitting in whitespace after a delimiter
        (when (and (not (eolp))
                   (not (eq (char-after) sep-char))
                   (save-excursion
                     (skip-syntax-backward " ")
                     (eq (char-before) sep-char)))
          (skip-syntax-forward " "))
        (adoc-addons--table-next-cell sep-char start end)))))

;;;###autoload
(defun adoc-addons-table-shift-tab ()
  "Move to the previous cell in an AsciiDoc table.
Falls back to `backward-char' when outside a table."
  (interactive)
  (let ((bounds (ignore-errors (adoc-addons--table-bounds))))
    (if (not bounds)
        (backward-char)
      (let* ((start (car bounds))
             (sep   (adoc-addons--table-separator start))
             (sep-char (string-to-char sep)))
        (when (save-excursion
                (beginning-of-line)
                (or (looking-at-p adoc-addons--table-block-delimiter)
                    (looking-at-p "^\\s-*$")))
          (forward-line -1)
          (end-of-line))
        ;; Normalise point when sitting in whitespace before a delimiter
        (when (and (not (bolp))
                   (not (eq (char-before) sep-char))
                   (save-excursion
                     (skip-syntax-forward " ")
                     (eq (char-after) sep-char)))
          (skip-syntax-backward " "))
        (adoc-addons--table-prev-cell sep-char start)))))

(provide 'adoc-addons)
;;; adoc-addons.el ends here
