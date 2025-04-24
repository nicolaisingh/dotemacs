;;; howm-shift.el --- Define org-mode-style S-<direction> functions for howm.

;; Copyright (C) 2025 Nicolai Singh

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

;; Extend howm by adding org-mode-style S-<up>, S-<down>, S-<left>,
;; S-<right> functions.  Only timestamps are supported as of the
;; moment.

;; Add bindings using:
;; (require 'howm-shift)
;; (keymap-set howm-mode-map "S-<down>" #'howm-shift-down)
;; (keymap-set howm-mode-map "S-<left>" #'howm-shift-left)
;; (keymap-set howm-mode-map "S-<right>" #'howm-shift-right)
;; (keymap-set howm-mode-map "S-<up>" #'howm-shift-up)

;;; Code:

(require 'howm)

(defun howm-shift-pos-in-match-range (pos n)
  "Return non-nil if POS is within the position of matched group N."
  (and (match-beginning n)
       (>= pos (match-beginning n))
       (<= pos (match-end n))))

(defun howm-shift-at-timestamp-p ()
  "Return non-nil if point is within a howm timestamp.

The actual return value can be one of the following symbols:
'year   : If point is within the year component
'month  : If point is within the month component
'day    : If point is within the day component, or in any other part of the timestamp
'hour   : If point is within the hour component
'minute : If point is within the minute component"
  (catch 'matched
    (save-excursion
      (let ((date-regexp "\\[\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\(?: \\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\)?\\]")
            (pos (point)))
        (forward-line 0)
        (while (and (re-search-forward date-regexp (pos-eol) t))
          (when (and (<= (match-beginning 0) pos)
                     (> (match-end 0) pos))
            (let ((component-matched (cond
                                      ((howm-shift-pos-in-match-range pos 1) 'year)
                                      ((howm-shift-pos-in-match-range pos 2) 'month)
                                      ((howm-shift-pos-in-match-range pos 3) 'day)
                                      ((howm-shift-pos-in-match-range pos 4) 'hour)
                                      ((howm-shift-pos-in-match-range pos 5) 'minute)
                                      (t 'day))))
              (throw 'matched component-matched))))))))

(defun howm-shift-ts-shift (n &optional component)
  "Shift a timestamp's COMPONENT by N.

See `howm-shift-at-timestamp-p' for the components supported."
  (let* ((pos (point))
         (howm-timestamp (match-string-no-properties 0))
         (year (string-to-number (match-string-no-properties 1)))
         (month (string-to-number (match-string-no-properties 2)))
         (day (string-to-number (match-string-no-properties 3)))
         (hour (let ((hstr (match-string-no-properties 4)))
                 (when hstr (string-to-number hstr))))
         (minute (let ((mstr (match-string-no-properties 5)))
                   (when mstr (string-to-number mstr))))
         (time-component-p (and hour minute))
         (second 0)
         (adjusted (encode-time
                    (cond ((eq component 'year)
                           (list second (or minute 0) (or hour 0) day month (+ year n)))
                          ((eq component 'month)
                           (list second (or minute 0) (or hour 0) day (+ month n) year))
                          ((eq component 'day)
                           (list second (or minute 0) (or hour 0) (+ day n) month year))
                          ((eq component 'hour)
                           (list second (or minute 0) (+ hour n) day month year))
                          ((eq component 'minute)
                           (list second (+ minute n) hour day month year))
                          (t
                           (list second (or minute 0) (or hour 0) (+ day n) month year)))))
         (new-ts (if time-component-p
                     (format-time-string "[%Y-%m-%d %H:%M]" adjusted)
                   (format-time-string "[%Y-%m-%d]" adjusted))))
    (delete-region (match-beginning 0) (match-end 0))
    (insert new-ts)
    (goto-char pos)))

(defun howm-shift-right (arg)
  (interactive "p")
  (cond ((howm-shift-at-timestamp-p) (howm-shift-ts-shift arg))
        (t (message "Nothing to update"))))

(defun howm-shift-left (arg)
  (interactive "p")
  (cond ((howm-shift-at-timestamp-p) (howm-shift-ts-shift (- arg)))
        (t (message "Nothing to update"))))

(defun howm-shift-up (arg)
  (interactive "p")
  (let ((at-timestamp-p (howm-shift-at-timestamp-p)))
    (cond (at-timestamp-p (howm-shift-ts-shift arg at-timestamp-p))
          (t (message "Nothing to update")))))

(defun howm-shift-down (arg)
  (interactive "p")
  (let ((at-timestamp-p (howm-shift-at-timestamp-p)))
    (cond (at-timestamp-p (howm-shift-ts-shift (- arg) at-timestamp-p))
          (t (message "Nothing to update")))))

(provide 'howm-shift)
;;; howm-shift.el ends here
