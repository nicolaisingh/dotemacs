;;; init-org.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package org.

;;; Code:

(require 'org)

(setq
 org-startup-indented t
 org-indent-mode-turns-on-hiding-stars nil
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
 org-edit-src-content-indentation 0)

(setq org-capture-templates '(("i" "Inbox" entry
                               (file+headline "~/org/inbox.org" "Inbox")
                               "* %?\n  %U" :empty-lines-before 1)
                              ("l" "Log" entry
                               (file+olp+datetree "~/org/log.org")
                               "* %?" :empty-lines 1)
                              ("D" "Distraction" item
                               (file "~/org/distraction.org")
                               "- %? %U")))

(setq org-agenda-custom-commands
      '(("Z" "MAtch blabla" tags "" ((org-use-tag-inheritance nil)))))

(setq org-tag-alist '(("@lit" . ?l)
                      ("@project" . ?p)
                      ("@ref" . ?r)))

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

(defun org-dired-open-notes ()
  (interactive)
  (dired "~/org/"))

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
(global-set-key (kbd "C-c o d") #'org-dired-open-notes)

(provide 'init-org)
;;; init-org.el ends here
