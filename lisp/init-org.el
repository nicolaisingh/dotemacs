;;; init-org.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package org.

;;; Code:

(require 'org)

(setq
 org-startup-indented nil
 org-indent-mode-turns-on-hiding-stars t
 org-default-notes-file "~/org/inbox.org"
 org-archive-location "archive/%s::"
 org-refile-targets '((nil :maxlevel . 2)
                      (org-agenda-files :maxlevel . 2)
                      ("~/org/log.org" :maxlevel . 3))
 org-complete-tags-always-offer-all-agenda-tags t
 org-refile-use-outline-path 'file
 org-outline-path-complete-in-steps nil
 org-adapt-indentation nil
 org-agenda-start-with-follow-mode t
 org-agenda-search-view-max-outline-level 2
 org-src-fontify-natively nil
 org-fontify-done-headline nil
 org-fontify-todo-headline nil
 org-edit-src-content-indentation 0
 org-tags-column 0
 org-auto-align-tags nil
 org-special-ctrl-a/e t
 org-special-ctrl-k t
 org-hide-emphasis-markers t
 org-hide-leading-stars t
 org-agenda-category-icon-alist '(("reading" ("📖"))
                                  ("routine" ("🔁"))
                                  ("ideate" ("💡"))))

(setq org-capture-templates '(("i" "Inbox" entry
                               (file+headline "~/org/inbox.org" "Inbox")
                               "* %?\n  %U" :empty-lines-before 1)
                              ("p" "Project" entry
                               (file "~/org/projects.org")
                               "* TODO %?\n  %U" :empty-lines-before 1)
                              ("D" "Distraction" item
                               (file "~/org/distraction.org")
                               "- %? %U")))

(setq org-agenda-custom-commands
      '(("I" "Ideate TODOs" tags-todo "ideate")
        ("P" "All project TODOs" tags-todo "@project")
        ("R" "All routines" ((tags-todo "+CATEGORY=\"routine\"")))))

(set-face-attribute 'org-level-1 t :height 1.15)
(set-face-attribute 'org-level-2 t :height 1.1)

(set-face-attribute 'org-drawer t
                    :inherit 'org-modern-label
                    :foreground "gray50")

(set-face-attribute 'org-special-keyword t
                    :height 0.8)

(set-face-attribute 'org-property-value t
                    :height 0.8
                    :foreground "gray50")

(set-face-attribute 'org-block-begin-line t
                    :inherit 'shadow
                    :background "gray96")

(set-face-attribute 'org-block-end-line t
                    :background "gray99")

(set-face-attribute 'org-block t
                    :background "gray99")

(set-face-attribute 'org-todo t
                    :foreground "coral3")

(with-eval-after-load 'org
  (require 'ox-md)
  (require 'ox-gfm)
  (require 'ox-jira)
  (require 'ox-slack))

(defun org-fixup-whitespace ()
  (interactive)
  (if (region-active-p)
      (org-indent-region (region-beginning) (region-end))
    (org-indent-region (point-min) (point-max)))
  ;; Call with a C-u prefix to fixup tag indentation
  (let ((current-prefix-arg '(4)))
    (call-interactively #'org-set-tags-command)))

(defun org-save-dest-buffer-after-refile ()
  (interactive)
  (save-window-excursion
    (org-refile-goto-last-stored)
    (call-interactively #'save-buffer)))
(add-hook 'org-after-refile-insert-hook #'org-save-dest-buffer-after-refile)

(defun org-refile-within-file ()
  (interactive)
  (let ((org-refile-targets '((nil :maxlevel . 3))))
    (call-interactively #'org-refile)))

(defun org-mode-my-custom-keys ()
  (define-key org-mode-map (kbd "C-c C--") #'org-ctrl-c-minus)
  (define-key org-mode-map (kbd "C-c C-8") #'org-ctrl-c-star)
  (define-key org-mode-map (kbd "C-c C-SPC") #'org-table-blank-field)
  (define-key org-mode-map (kbd "C-c C-S-W") #'org-refile-within-file)
  (define-key org-mode-map (kbd "C-M-q") #'org-fixup-whitespace)
  (define-key org-mode-map (kbd "C-M-h") #'org-mark-subtree)
  ;; requires consult
  (define-key org-mode-map (kbd "C-c *") #'consult-org-heading))
(add-hook 'org-mode-hook #'org-mode-my-custom-keys)
;; (add-hook 'org-mode-hook #'turn-on-auto-fill)
(add-hook 'org-mode-hook #'indent-spaces)
(add-hook 'org-mode-hook #'final-newline)
(add-hook 'org-agenda-mode-hook #'hl-line-mode)
(add-hook 'org-mode-hook #'virtual-auto-fill-mode)

(global-set-key (kbd "C-c o l") #'org-store-link)
(global-set-key (kbd "C-c o c") #'org-capture)
(global-set-key (kbd "C-c o a") #'org-agenda)
(global-set-key (kbd "C-c o b") #'org-switchb)
(global-set-key (kbd "C-c o v") #'visible-mode)

(provide 'init-org)
;;; init-org.el ends here
