;;; init-org.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package org.

;;; Code:

(require 'org)

(defun org-refile-target-projects ()
  (directory-files "~/org/projects" t directory-files-no-dot-files-regexp))

(setq
 org-startup-indented nil
 org-indent-mode-turns-on-hiding-stars t
 org-default-notes-file "~/org/inbox.org"
 org-archive-location "archive/%s::"
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
 org-hide-emphasis-markers nil
 org-hide-leading-stars t
 org-log-into-drawer t
 org-agenda-category-icon-alist '()
 org-startup-with-inline-images t
 org-cycle-inline-images-display t
 org-image-actual-width 550
 org-attach-expert nil
 org-attach-store-link-p 'attached
 ;; Allows viewing attachments when archived
 org-attach-id-dir "~/org/data/"

 org-refile-targets '((nil :maxlevel . 1)
                      ("~/org/inbox.org" :todo . "TOPIC")
                      (org-refile-target-projects :todo . "INBOX")
                      (org-refile-target-projects :todo . "TODO"))

 org-todo-keywords '((sequence "TODO(t)" "WIP(p)" "DEFERRED(f@)" "WAITING(w@)"
                               "|" "DONE(d@/@)" "CANCELED(c@/@)")
                     (type "INBOX(i)" "TOPIC(o)"))

 org-todo-keyword-faces '(("WIP" :foreground "tomato" :weight bold)
                          ("DEFERRED" :foreground "dark magenta" :weight bold)
                          ("WAITING" :foreground "orange" :weight bold)
                          ("CANCELED" :foreground "dark gray" :weight bold)
                          ("INBOX" :foreground "dark slate blue" :weight bold)
                          ("TOPIC" :foreground "dark slate blue" :weight bold)))

(setq org-capture-templates '(("i" "Inbox" entry (file org-default-notes-file)
                               "* %?\n:PROPERTIES:\n:CREATED:  %U\n:END:"
                               :empty-lines 1
                               :prepend t)
                              ("t" "Todo" entry
                               (file org-default-notes-file)
                               "* TODO %?\n:PROPERTIES:\n:CREATED:  %U\n:END:"
                               :empty-lines 1
                               :prepend t)))

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

(defun org-refile-to-topic ()
  (interactive)
  (let ((org-refile-targets '((nil :todo . "TOPIC"))))
    (call-interactively #'org-refile)))

(defun org-link-retain-description ()
  "Delete an org-mode-link and retain only the description."
  (interactive)
  (let* ((element (org-element-context))
         (element-type (car element)))
    (when (eq 'link element-type)
      (let* ((contents (buffer-substring (org-element-property :contents-begin element)
                                         (org-element-property :contents-end element))))
        (delete-region (org-element-property :begin element)
                       (org-element-property :end element))
        (insert contents)))))

(defun my-org-attach ()
  "Use Downloads as the default target directory."
  (interactive)
  (let ((dired-dwim-target (lambda ()
                             (list "~/Downloads/"))))
    (call-interactively #'org-attach)))

(defun org-mode-my-custom-keys ()
  (define-key org-mode-map (kbd "C-c C-a") #'my-org-attach)
  (define-key org-mode-map (kbd "C-c C--") #'org-ctrl-c-minus)
  (define-key org-mode-map (kbd "C-c C-8") #'org-ctrl-c-star)
  (define-key org-mode-map (kbd "C-c C-SPC") #'org-table-blank-field)
  (define-key org-mode-map (kbd "C-c C-S-W") #'org-refile-to-topic)
  (define-key org-mode-map (kbd "C-M-q") #'org-fixup-whitespace)
  (define-key org-mode-map (kbd "C-M-h") #'org-mark-subtree)
  (define-key org-mode-map (kbd "C-c o L") #'org-link-retain-description)
  ;; requires consult
  (define-key org-mode-map (kbd "C-c *") #'consult-org-heading))

(add-hook 'org-mode-hook #'org-mode-my-custom-keys)
;; (add-hook 'org-mode-hook #'turn-on-auto-fill)
(add-hook 'org-mode-hook #'indent-spaces)
(add-hook 'org-mode-hook #'final-newline)
(add-hook 'org-agenda-mode-hook #'hl-line-mode)
(add-hook 'org-mode-hook #'virtual-auto-fill-mode)

;; Always put an ID in new nodes so org-roam can recognize them
(add-hook 'org-capture-before-finalize-hook #'org-id-get-create)

(defun org-capture-inbox (goto)
  (interactive "P")
  (org-capture goto "i"))

(defun org-dired ()
  (interactive)
  (dired org-directory))

(global-set-key (kbd "C-c o s") #'org-store-link)
(global-set-key (kbd "C-c o a") #'org-agenda)
(global-set-key (kbd "C-c o b") #'org-switchb)
(global-set-key (kbd "C-c o v") #'visible-mode)
(global-set-key (kbd "C-c o d") #'org-dired)

(provide 'init-org)
;;; init-org.el ends here
