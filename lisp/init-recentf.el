;;; init-recentf.el --- Init customization

;;; Commentary:

;; This contains init customization for the package recentf.

;;; Code:

(require 'recentf)

(add-hook 'after-init-hook #'recentf-mode)

(setq recentf-max-menu-items 1000
      recentf-max-saved-items 10000
      recentf-auto-cleanup 120
      recentf-menu-filter 'recentf-show-basenames-ascending)

(add-to-list 'recentf-exclude "\\/sudoedit:root")

;; Save the recentf list every 10 minutes
(run-at-time nil (* 10 60) 'recentf-save-list)

(defun recentf-open-files-completing-read ()
  (interactive)
  (let* ((recentf-menu (recentf-make-menu-items))
         (menu-labels (mapcar (lambda (elt) (aref elt 0))
                              recentf-menu))
         (result (completing-read "Open recent: " menu-labels))
         (selection-pos (cl-position result menu-labels :test 'equal))
         (recentf-form (aref (nth selection-pos recentf-menu) 1)))
    (cond ((functionp recentf-form)
           ;; recentf uses lambdas to find-file an item
           (funcall recentf-form))
          ((eq 'customize-group (car recentf-form))
           ;; The item "More..." needs to be eval'd
           (eval recentf-form)))))

(global-set-key (kbd "C-x C-S-f") #'recentf-open-files-completing-read)

(provide 'init-recentf)
;;; init-recentf.el ends here
