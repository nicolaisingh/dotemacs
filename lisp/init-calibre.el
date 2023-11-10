;;; init-calibre.el --- Init customization

;;; Commentary:

;; This contains init customization for calibre.

;;; Code:

(require 'calibre)

(setq calibre-libraries '(("library" . "~/calibre")))
(global-set-key (kbd "C-c L") #'calibre-library)

(provide 'init-calibre)
;;; init-calibre.el ends here
