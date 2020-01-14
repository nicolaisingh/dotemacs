;;; init-org.el --- Init configuration.

;;; Commentary:

;; This contains init configuration for the package org.

;;; Code:

(require 'org)

(setq org-startup-indented 1
      org-default-notes-file (concat org-directory "/notes.org")
      org-refile-targets '((nil :maxlevel . 3)
			   (org-agenda-files :maxlevel . 2)))

(setq org-capture-templates '(("i" "Just some item" entry
			       (file+headline "~/org/gtd.org" "Inbox")
			       "* %?")))

(setq org-tag-alist '(("@dev" . ?d)
		      ("@idea" . ?i)
		      ("@learn" . ?l)
		      ("@home" . ?h)))

(global-set-key (kbd "C-c o l") 'org-store-link)
(global-set-key (kbd "C-c o c") 'org-capture)
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o b") 'org-switchb)

(provide 'init-org)
;;; init-org.el ends here
