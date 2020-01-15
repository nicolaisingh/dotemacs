;;; init-package.el --- Init configuration.

;;; Commentary:

;; This contains init configuration for package.el, including adding
;; MELPA to the package archives.

;;; Code:

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("ox-odt" . "https://kjambunathan.github.io/elpa/"))
(package-initialize)

(provide 'init-package)
;;; init-package.el ends here
