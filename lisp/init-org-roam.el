;;; init-org-roam.el --- Init customization

;;; Commentary:

;; This contains init customization for org-roam.

;;; Code:

(require 'org-roam)
(require 'org-roam-dailies)

(setq org-roam-directory "~/org/roam"
      org-roam-db-location "~/org/org-roam.db"
      org-roam-completion-everywhere t
      org-roam-node-display-template (concat
                                      "${title:100} "
                                      (propertize "${tags}" 'face 'org-tag)
                                      "${mytodo}")
      org-roam-dailies-capture-templates '(("d" "default" entry "* %?"
                                            :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")
                                            :empty-lines-before 1)))

(set-face-attribute 'org-roam-dailies-calendar-note nil :weight 'bold)

(cl-defmethod org-roam-node-mytodo ((node org-roam-node))
  (let ((todo (org-roam-node-todo node)))
    (when todo
      (message "%s" todo)
      (concat
       " "
       (cond ((equal todo "DONE") (propertize todo 'face 'org-done))
             (t (propertize todo 'face 'org-todo)))))))

(setq org-roam-capture-templates
      '(("d" "default" plain "%?" :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                     "#+title: ${title}\n")
         :empty-lines-before 1
         :unnarrowed t)
        ("l" "literature" plain "%?"
         :target (file+head "literature/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+filetags: :@lit:\n#+title: ${title}")
         :empty-lines-before 1
         :unnarrowed t)
        ("x" "index" plain "%?"
         :target (file+head "index/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+filetags: :@lit:index:\n#+title: ${title}\n#+author: %^{author}")
         :empty-lines-before 1
         :unnarrowed t)

        ("r" "refs" plain "%?"
         :target (file+head "refs/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+filetags: :@ref:\n#+title: ${title}")
         :empty-lines-before 1
         :unnarrowed t)

        ("i" "ideate" plain "%?"
         :target (file+head "ideate/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+filetags: :ideate:\n#+title: ${title}")
         :empty-lines-before 1
         :unnarrowed t)))

(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . left)
               (window-width . 0.40)
               (window-height . fit-window-to-buffer)))

(org-roam-db-autosync-mode)

(defun org-roam-node-insert-immediate-finish ()
  (interactive)
  (let ((org-roam-capture-templates (mapcar (lambda (elt)
                                              (append elt '(:immediate-finish t)))
                                            org-roam-capture-templates)))
    (call-interactively #'org-roam-node-insert)))

(let ((map global-map))
  (define-key map (kbd "C-c n l") #'org-roam-buffer-toggle)
  (define-key map (kbd "C-c n f") #'org-roam-node-find)
  (define-key map (kbd "C-c n i") #'org-roam-node-insert)
  (define-key map (kbd "C-c n I") #'org-roam-node-insert-immediate-finish)
  (define-key map (kbd "C-c n c") #'org-roam-capture)
  (define-key map (kbd "C-c n g") #'org-roam-graph)
  (define-key map (kbd "C-c n a") #'org-roam-alias-add)
  (define-key map (kbd "C-c n A") #'org-roam-alias-remove)
  (define-key map (kbd "C-c n t") #'org-roam-tag-add)
  (define-key map (kbd "C-c n T") #'org-roam-tag-remove)
  (define-key map (kbd "C-c n n") #'org-id-get-create)
  (define-key map (kbd "C-c n r") #'org-roam-ref-add)
  (define-key map (kbd "C-c n R") #'org-roam-ref-remove)
  (define-key map (kbd "C-c n x") #'org-roam-extract-subtree)
  (define-key map (kbd "C-c n j") #'org-roam-dailies-capture-today)
  (define-key map (kbd "C-c n J") #'org-roam-dailies-capture-date))

(provide 'init-org-roam)
;;; init-org-roam.el ends here
