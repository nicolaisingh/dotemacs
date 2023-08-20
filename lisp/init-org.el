;;; init-org.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package org.

;;; Code:

(require 'org)

(setq
 org-startup-indented nil
 org-default-notes-file "~/org/inbox.org"
 org-refile-targets '((nil :maxlevel . 3)
                      (org-agenda-files :maxlevel . 2))
 org-complete-tags-always-offer-all-agenda-tags t
 org-refile-use-outline-path 'file
 org-outline-path-complete-in-steps nil
 org-adapt-indentation t)

(setq org-capture-templates '(("n" "Notes inbox" entry
                               (file+headline "~/org/inbox.org" "Notes")
                               "* %^{Content|Reminder}\n\n%U\n%?" :empty-lines 1)
                              ("p" "Projects inbox" entry
                               (file+headline "~/org/inbox.org" "Projects")
                               "* %^{Content|Reminder}\n\n%U\n%?" :empty-lines 1)
                              ("l" "Log" entry
                               (file+olp+datetree "~/org/log.org")
                               "* %?" :empty-lines 1)
                              ("D" "Distraction" item
                               (file "~/org/distraction.org")
                               "- %? %U")))

(setq org-tag-alist '(("@Lit" . ?l)
                      ("@Project" . ?p)
                      ("@Task" . ?t)))

(defun org-fixup-indents ()
  (interactive)
  (org-indent-region (point-min) (point-max))
  ;; Call with a C-u prefix to fixup tag indentation
  (let ((current-prefix-arg '(4)))
    (call-interactively #'org-set-tags-command)))

(defun org-mode-my-custom-keys ()
  (define-key org-mode-map (kbd "C-c C--") #'org-ctrl-c-minus)
  (define-key org-mode-map (kbd "C-c C-8") #'org-ctrl-c-star)
  (define-key org-mode-map (kbd "C-c C-SPC") #'org-table-blank-field))
(add-hook 'org-mode-hook #'org-mode-my-custom-keys)
(add-hook 'org-mode-hook #'turn-on-auto-fill)
(add-hook 'org-mode-hook #'indent-spaces)

(global-set-key (kbd "C-c o l") #'org-store-link)
(global-set-key (kbd "C-c o c") #'org-capture)
(global-set-key (kbd "C-c o a") #'org-agenda)
(global-set-key (kbd "C-c o b") #'org-switchb)
(global-set-key (kbd "C-c o v") #'visible-mode)

(provide 'init-org)
;;; init-org.el ends here
