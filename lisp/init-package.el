;;; init-package.el --- Init configuration.

;;; Commentary:

;; This contains init configuration for package.el, including adding
;; MELPA to the package archives.

;;; Code:

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("ox-odt" . "https://kjambunathan.github.io/elpa/"))

(when (< emacs-major-version 27)
  (package-initialize))

(defun ensure-all-packages-installed ()
  (dolist (package package-selected-packages)
    (unless (package-installed-p package)
      (message "Installing missing package `%s'" package)
      (package-install package))))

(provide 'init-package)
;;; init-package.el ends here
