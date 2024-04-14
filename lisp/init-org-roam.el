;;; init-org-roam.el --- Init customization

;;; Commentary:

;; This contains init customization for org-roam.

;;; Code:

(require 'org-roam)

(defvar org-roam-content-width 50)

;; For customizing the faces
(require 'org-roam-dailies)
(require 'org-roam-mode)

(setq org-roam-directory "~/org"
      org-roam-db-location "~/org/org-roam.db"
      org-roam-completion-everywhere t
      org-roam-node-display-template (concat
                                      "${title:100} "
                                      (propertize "${tags}" 'face 'org-tag)
                                      "${mytodo}")
      org-roam-dailies-capture-templates '(("d" "default" entry "* %?"
                                            :target (file+head "%<%Y-%m-%d>.org" "#+title: Journal - %<%Y-%m-%d>\n")
                                            :empty-lines-before 1)))

(set-face-attribute 'org-roam-dailies-calendar-note nil :weight 'bold)
(set-face-attribute 'org-roam-title nil
                    :weight 'bold
                    :box '(:style released-button)
                    :foreground "slate blue")

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
        ("p" "project" plain "%?"
         :target (file+head "projects/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+filetags: :@project:\n#+title: ${title}")
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

        ("I" "ideate" plain "%?"
         :target (file+head "ideate/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+filetags: :ideate:\n#+title: ${title}")
         :empty-lines-before 1
         :unnarrowed t)))

(add-to-list 'display-buffer-alist
             `("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . left)
               (window-width . ,(+ 5 org-roam-content-width))
               (window-height . fit-window-to-buffer)))

(org-roam-db-autosync-mode)

(defun my-org-roam-extract-subtree ()
  "Use the first tag in a node as its destination within `org-roam-directory'."
  (interactive)
  (let* ((first-tag (car (org-get-tags)))
         (relative-path (if first-tag (concat "/" first-tag "/")
                          ""))
         (org-roam-directory (concat org-roam-directory relative-path)))
    (org-roam-extract-subtree)))

(defun org-roam-node-insert-immediate-finish ()
  (interactive)
  (let ((org-roam-capture-templates (mapcar (lambda (elt)
                                              (append elt '(:immediate-finish t)))
                                            org-roam-capture-templates)))
    (call-interactively #'org-roam-node-insert)))

(defun org-roam-dailies-goto-previous-note-repeatable ()
  "Call `org-roam-dailies-goto-previous-note' using `repeatkey-repeatable-call'."
  (interactive)
  (repeatkey-repeatable-call #'org-roam-dailies-goto-previous-note
                             '(("." . org-roam-dailies-goto-next-note-repeatable)
                               ("," . org-roam-dailies-goto-previous-note-repeatable))))

(defun org-roam-dailies-goto-next-note-repeatable ()
  "Call `org-roam-dailies-goto-next-note' using `repeatkey-repeatable-call'."
  (interactive)
  (repeatkey-repeatable-call #'org-roam-dailies-goto-next-note
                             '(("." . org-roam-dailies-goto-next-note-repeatable)
                               ("," . org-roam-dailies-goto-previous-note-repeatable))))

(let ((map global-map))
  (define-key map (kbd "C-c n f") #'org-roam-node-find)
  (define-key map (kbd "C-c n c") #'org-roam-capture)
  (define-key map (kbd "C-c n g") #'org-roam-graph)
  (define-key map (kbd "C-c n %") #'org-roam-node-random)
  (define-key map (kbd "C-c n j") #'org-roam-dailies-capture-today)
  (define-key map (kbd "C-c n J") #'org-roam-dailies-capture-date)
  (define-key map (kbd "C-c n y") #'org-roam-dailies-goto-yesterday)
  (define-key map (kbd "C-c n .") #'org-roam-dailies-goto-next-note-repeatable)
  (define-key map (kbd "C-c n ,") #'org-roam-dailies-goto-previous-note-repeatable))

(let ((map org-mode-map))
  (define-key map (kbd "C-c n l") #'org-roam-buffer-toggle)
  (define-key map (kbd "C-c n i") #'org-roam-node-insert)
  (define-key map (kbd "C-c n I") #'org-roam-node-insert-immediate-finish)
  (define-key map (kbd "C-c n a") #'org-roam-alias-add)
  (define-key map (kbd "C-c n A") #'org-roam-alias-remove)
  (define-key map (kbd "C-c n t") #'org-roam-tag-add)
  (define-key map (kbd "C-c n T") #'org-roam-tag-remove)
  (define-key map (kbd "C-c n n") #'org-id-get-create)
  (define-key map (kbd "C-c n r") #'org-roam-ref-add)
  (define-key map (kbd "C-c n R") #'org-roam-ref-remove)
  (define-key map (kbd "C-c n x") #'org-roam-extract-subtree)
  (define-key map (kbd "C-c n X") #'my-org-roam-extract-subtree))

(let ((map org-roam-mode-map))
  (define-key map (kbd "C-c n l") #'org-roam-buffer-toggle))

(add-hook 'org-roam-mode-hook (lambda ()
                                (virtual-auto-fill-mode)
                                (set-fill-column org-roam-content-width)))

(provide 'init-org-roam)
;;; init-org-roam.el ends here
