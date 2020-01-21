;;; init-icomplete.el --- Init customization.

;;; Commentary:

;; This contains init customization for the package icomplete.

;;; Code:

(require 'icomplete)

(icomplete-mode 1)

(setq icomplete-prospects-height 1
      icomplete-separator "  |  "
      icomplete-hide-common-prefix nil)

;; Show prospects immediately
(setq icomplete-show-matches-on-no-input 1
      icomplete-compute-delay 0)

;; (add-to-list 'completion-styles 'initials)
;; (add-to-list 'completion-styles 'substring)

(let ((map icomplete-minibuffer-map))
  (define-key map (kbd "C-S-j") 'minibuffer-force-complete))

(when (>= emacs-major-version 27)
  (add-to-list 'completion-styles 'flex))

(provide 'init-icomplete)
;;; init-icomplete.el ends here
