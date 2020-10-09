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

;; Additional bs config that shows files, some buffer names and modes.
(add-to-list 'bs-configurations
             '("files-plus-some-buffers-and-modes"
               "^\\(\\*scratch\\*\\|test\\)$" bs-must-show-modes
               nil bs-visits-non-file
               bs-sort-buffer-interns-are-last))

(setq bs-default-configuration "files-plus-some-buffers-and-modes"
      bs-max-window-height 30
      bs-minimal-buffer-name-column 25)

;; bs-default-sort-name is not working
;; (setq bs-default-sort-name "by filename")
(setq bs--current-sort-function (assoc "by filename" bs-sort-functions))

(global-set-key (kbd "C-x C-b") #'bs-show)
(global-set-key (kbd "C-=") #'bs-cycle-next)
(global-set-key (kbd "C-+") #'bs-cycle-previous)

(add-hook 'bs-mode-hook #'hl-line-mode)

(provide 'init-bs)
;;; init-bs.el ends here
