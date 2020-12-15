;;; init-bs.el --- Init configuration

;;; Commentary:

;; This contains other init configuration for the package bs.

;;; Code:

(require 'bs)

(defun bs-must-show-modes (buf)
  "Return non-nil for buffers matching specific modes.  Used for
the configuration 'files-plus-some-buffers-and-modes."
  (let ((major-mode (buffer-local-value 'major-mode buf)))
    (find major-mode '(term-mode shell-mode fundamental-mode))))

(defun bs-dont-show-modes (buf)
  "Return non-nil for buffers which should not be shown."
  (or (bs-visits-non-file buf)
      (let ((major-mode (buffer-local-value 'major-mode buf)))
        (find major-mode '(dired-mode)))))

(defun bs-not-dired-mode-p (buf)
  "Return non-nil for dired buffers."
  (not (let ((major-mode (buffer-local-value 'major-mode buf)))
         (find major-mode '(dired-mode)))))

;; Additional bs configurations
;;;; Show files, some buffer names and modes
(add-to-list 'bs-configurations
             '("files-plus-some-buffers-and-modes"
               ;; Must show regexp and function
               "^\\(\\*scratch\\*\\|test\\)$"
               bs-must-show-modes
               ;; Don't show regexp and function
               "^\\(\\*Ilist\\*\\|\\*Messages\\*\\)$"
               bs-dont-show-modes
               ;; Sort function
               bs-sort-buffer-interns-are-last))

;;;; Show only dired buffers
(add-to-list 'bs-configurations
             '("dired-only"
               nil
               nil
               nil
               bs-not-dired-mode-p
               nil))

(setq bs-default-configuration "files-plus-some-buffers-and-modes"
      bs-max-window-height 30
      bs-minimal-buffer-name-column 25)

;; bs-default-sort-name is not working
;; (setq bs-default-sort-name "by filename")
;; (setq bs--current-sort-function (assoc "by filename" bs-sort-functions))

(global-set-key (kbd "C-x C-b") #'bs-show)
(global-set-key (kbd "C-=") #'bs-cycle-next)
(global-set-key (kbd "C-+") #'bs-cycle-previous)

(add-hook 'bs-mode-hook #'hl-line-mode)

(provide 'init-bs)
;;; init-bs.el ends here
