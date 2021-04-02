;;; init-bs.el --- Init configuration

;;; Commentary:

;; This contains other init configuration for the package bs.

;;; Code:

(require 'bs)

(defun bs-must-show-modes (buf)
  "Return non-nil for buffers matching specific modes.  Used for
the configuration 'files-plus-some-buffers-and-modes."
  (let ((major-mode (buffer-local-value 'major-mode buf)))
    (find major-mode '(term-mode shell-mode eshell-mode fundamental-mode))))

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
             '("default--files-plus-some-buffers-and-modes"
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

(setq bs-default-configuration "default--files-plus-some-buffers-and-modes"
      bs-max-window-height 35
      bs-minimal-buffer-name-column 20)

;; bs-default-sort-name is not working
;; (setq bs-default-sort-name "by filename")
;; (setq bs--current-sort-function (assoc "by filename" bs-sort-functions))

;; Custom size column based on file size, not buffer size
(defun bs--get-filesize-string (_start-buffer _all-buffers)
  "Return file size of the current buffer for Buffer Selection Menu."
  (let* ((attributes (file-attributes (buffer-name)))
         (filesize (if attributes (file-attribute-size attributes) 0))
         (a-megabyte (* 1024 1024)))
    (if (>= filesize a-megabyte)
        (concat (format "%.01f" (/ (float filesize) a-megabyte)) "M")
      (concat (format "%.01f" (/ (float filesize) 1024)) "K"))))

(defun bs--sort-by-filesize (b1 b2)
  (let* ((b1-attributes (file-attributes (buffer-file-name b1)))
         (b2-attributes (file-attributes (buffer-file-name b2)))
         (b1-filesize (if b1-attributes (file-attribute-size b1-attributes) 0))
         (b2-filesize (if b2-attributes (file-attribute-size b2-attributes) 0)))
    (< b1-filesize b2-filesize)))

(setq bs-attributes-list
      '((""       1   1 left  bs--get-marked-string)
        ("M"      1   1 left  bs--get-modified-string)
        ("R"      2   2 left  bs--get-readonly-string)
        ("Buffer" bs--get-name-length 10 left  bs--get-name)
        (""       1   1 left  " ")
        ;; ("Size"   8   8 right bs--get-size-string)
        ;; (""       1   1 left  " ")
        ("Size" 8   8 right bs--get-filesize-string)
        (""       1   1 left  " ")
        ("Mode"   12 12 right bs--get-mode-name)
        (""       2   2 left  "  ")
        ("File"   12 12 left  bs--get-file-name)
        (""       2   2 left  "  ")))

(setq bs-sort-functions
      '(("by name"     bs--sort-by-name     "Buffer" region)
        ;; ("by size"     bs--sort-by-size     "Size"   region)
        ("by filesize" bs--sort-by-filesize "Size"  region)
        ("by mode"     bs--sort-by-mode     "Mode"   region)
        ("by filename" bs--sort-by-filename "File"   region)
        ("by nothing"  nil                  nil      nil)))

(global-set-key (kbd "C-x C-b") #'bs-show)

(add-hook 'bs-mode-hook #'hl-line-mode)

(provide 'init-bs)
;;; init-bs.el ends here
