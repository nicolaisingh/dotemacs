;;; init-icomplete.el --- Init customization

;;; Commentary:

;; This contains init customization related to completion.

;;; Code:

(require 'icomplete)

(setq icomplete-prospects-height 1
      icomplete-separator (propertize "  |  " 'face 'font-lock-variable-name-face)
      icomplete-compute-delay 0
      completion-auto-help nil
      completion-cycle-threshold nil
      completion-pcm-complete-word-inserts-delimiters t)

(define-key minibuffer-local-filename-completion-map (kbd "SPC")
  #'minibuffer-complete-word)

(defun star-before-word-completion ()
  "Insert a literal `*' on the first invocation, then runs minibuffer-complete-word if invoked again.
Useful for completion style 'partial-completion."
  (interactive)
  (let ((prev-char (buffer-substring (- (point) 1) (point)))
        (replace-prev-char (lambda (char) (delete-char -1) (insert-char char))))
    (if (or (equal prev-char "-") (equal prev-char "*"))
        (minibuffer-complete-word)
      (insert-char ?*))))

(cond
 ((>= emacs-major-version 27)
  ;; fido-mode matches most of my icomplete preferences and it also
  ;; sets the same vars in the `t' condition

  ;; Emacs 28 introduces `fido-vertical-mode'
  (if (= emacs-major-version 27)
      (fido-mode t)
    (fido-vertical-mode t))

  (defun prefer-pcm-before-flex ()
    (setq-local completion-styles '(partial-completion flex)))

  (defun icomplete-my-custom-keys ()
    (let ((map icomplete-minibuffer-map))
      (define-key map (kbd "SPC") #'star-before-word-completion)
      (define-key map (kbd "C-S-j") #'icomplete-force-complete)
      (define-key map (kbd "C-n") #'icomplete-forward-completions)
      (define-key map (kbd "C-p") #'icomplete-backward-completions)
      (define-key map (kbd "C-?") (lambda ()
                                    (interactive)
                                    (minibuffer-hide-completions)))
      (define-key map (kbd "C-^") (lambda ()
                                    (interactive)
                                    (setq-local max-mini-window-height
                                                (if (< max-mini-window-height 0.3)
                                                    0.3 0.75))))))

  (defun icomplete-set-vertical-height ()
    (setq-local max-mini-window-height 0.15))

  (add-hook 'icomplete-minibuffer-setup-hook #'prefer-pcm-before-flex)
  (add-hook 'icomplete-minibuffer-setup-hook #'icomplete-my-custom-keys)
  (add-hook 'icomplete-minibuffer-setup-hook #'icomplete-set-vertical-height))

 (t
  ;; Original preferences prior to Emacs 27
  (icomplete-mode t)

  (setq icomplete-hide-common-prefix nil
        icomplete-show-matches-on-no-input t
        completion-ignore-case t
        read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t)

  (let ((map icomplete-minibuffer-map))
    (define-key map (kbd "C-S-j") #'minibuffer-force-complete)
    (define-key map (kbd "SPC") #'star-before-word-completion))

  ;; TEMPORARY: There's a bug in icomplete where the `.' and `..'
  ;; entries are not being shown in the completion list
  ;; - https://lists.gnu.org/archive/html/bug-gnu-emacs/2018-09/msg00444.html
  ;; - https://lists.gnu.org/archive/html/bug-gnu-emacs/2019-01/msg00365.html
  (defun icomplete-completions (name candidates predicate require-match)
    "Identify prospective candidates for minibuffer completion.

The display is updated with each minibuffer keystroke during
minibuffer completion.

Prospective completion suffixes (if any) are displayed, bracketed by
one of (), [], or {} pairs.  The choice of brackets is as follows:

  (...) - a single prospect is identified and matching is enforced,
  [...] - a single prospect is identified but matching is optional, or
  {...} - multiple prospects, separated by commas, are indicated, and
          further input is required to distinguish a single one.

If there are multiple possibilities, `icomplete-separator' separates them.

The displays for unambiguous matches have ` [Matched]' appended
\(whether complete or not), or ` [No matches]', if no eligible
matches exist."
    (let* ((minibuffer-completion-table candidates)
           (minibuffer-completion-predicate predicate)
           (md (completion--field-metadata (icomplete--field-beg)))
           (comps (completion-all-sorted-completions
                   (icomplete--field-beg) (icomplete--field-end)))
           (last (if (consp comps) (last comps)))
           (base-size (cdr last))
           (open-bracket (if require-match "(" "["))
           (close-bracket (if require-match ")" "]")))
      ;; `concat'/`mapconcat' is the slow part.
      (if (not (consp comps))
          (progn ;;(debug (format "Candidates=%S field=%S" candidates name))
            (format " %sNo matches%s" open-bracket close-bracket))
        (if last (setcdr last nil))
        ;; (when (and minibuffer-completing-file-name
        ;; icomplete-with-completion-tables)
        ;; (setq comps (completion-pcm--filename-try-filter comps)))
        (let* ((most-try
                (if (and base-size (> base-size 0))
                    (completion-try-completion
                     name candidates predicate (length name) md)
                  ;; If the `comps' are 0-based, the result should be
                  ;; the same with `comps'.
                  (completion-try-completion
                   name comps nil (length name) md)))
               (most (if (consp most-try) (car most-try)
                       (if most-try (car comps) "")))
               ;; Compare name and most, so we can determine if name is
               ;; a prefix of most, or something else.
               (compare (compare-strings name nil nil
                                         most nil nil completion-ignore-case))
               (ellipsis (if (char-displayable-p ?…) "…" "..."))
               (determ (unless (or (eq t compare) (eq t most-try)
                                   (= (setq compare (1- (abs compare)))
                                      (length most)))
                         (concat open-bracket
                                 (cond
                                  ((= compare (length name))
                                   ;; Typical case: name is a prefix.
                                   (substring most compare))
                                  ;; Don't bother truncating if it doesn't gain
                                  ;; us at least 2 columns.
                                  ((< compare (+ 2 (string-width ellipsis))) most)
                                  (t (concat ellipsis (substring most compare))))
                                 close-bracket)))
               ;;"-prospects" - more than one candidate
               (prospects-len (+ (string-width
                                  (or determ (concat open-bracket close-bracket)))
                                 (string-width icomplete-separator)
                                 (+ 2 (string-width ellipsis)) ;; take {…} into account
                                 (string-width (buffer-string))))
               (prospects-max
                ;; Max total length to use, including the minibuffer content.
                (* (+ icomplete-prospects-height
                      ;; If the minibuffer content already uses up more than
                      ;; one line, increase the allowable space accordingly.
                      (/ prospects-len (window-width)))
                   (window-width)))
               ;; Find the common prefix among `comps'.
               ;; We can't use the optimization below because its assumptions
               ;; aren't always true, e.g. when completion-cycling (bug#10850):
               ;; (if (eq t (compare-strings (car comps) nil (length most)
               ;;              most nil nil completion-ignore-case))
               ;;     ;; Common case.
               ;;     (length most)
               ;; Else, use try-completion.
               (prefix (when icomplete-hide-common-prefix
                         (try-completion "" comps)))
               (prefix-len
                (and (stringp prefix)
                     ;; Only hide the prefix if the corresponding info
                     ;; is already displayed via `most'.
                     (string-prefix-p prefix most t)
                     (length prefix))) ;;)
               prospects comp limit)
          (if (or (eq most-try t) (not (consp (cdr comps))))
              (setq prospects nil)
            (when (member name comps)
              ;; NAME is complete but not unique.  This scenario poses
              ;; following UI issues:
              ;;
              ;; - When `icomplete-hide-common-prefix' is non-nil, NAME
              ;;   is stripped empty.  This would make the entry
              ;;   inconspicuous.
              ;;
              ;; - Due to sorting of completions, NAME may not be the
              ;;   first of the prospects and could be hidden deep in
              ;;   the displayed string.
              ;;
              ;; - Because of `icomplete-prospects-height' , NAME may
              ;;   not even be displayed to the user.
              ;;
              ;; To circumvent all the above problems, provide a visual
              ;; cue to the user via an "empty string" in the try
              ;; completion field.
              (setq determ (concat open-bracket "" close-bracket)))
            ;; Compute prospects for display.
            (while (and comps (not limit))
              (setq comp
                    (if prefix-len (substring (car comps) prefix-len) (car comps))
                    comps (cdr comps))
              (setq prospects-len
                    (+ (string-width comp)
                       (string-width icomplete-separator)
                       prospects-len))
              (if (< prospects-len prospects-max)
                  (push comp prospects)
                (setq limit t))))
          (setq prospects (nreverse prospects))
          ;; Decorate first of the prospects.
          (when prospects
            (let ((first (copy-sequence (pop prospects))))
              (put-text-property 0 (length first)
                                 'face 'icomplete-first-match first)
              (push first prospects)))
          ;; Restore the base-size info, since completion-all-sorted-completions
          ;; is cached.
          (if last (setcdr last base-size))
          (if prospects
              (concat determ
                      "{"
                      (mapconcat 'identity prospects icomplete-separator)
                      (and limit (concat icomplete-separator ellipsis))
                      "}")
            (concat determ " [Matched]"))))))))


(provide 'init-icomplete)
;;; init-icomplete.el ends here
