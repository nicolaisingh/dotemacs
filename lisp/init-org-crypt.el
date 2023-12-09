;;; init-org-crypt.el --- Init customization

;;; Commentary:

;; This contains init customization for org-crypt.

;;; Code:

(require 'org-crypt)

(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance '("crypt")
      org-crypt-key "0xA4F3599BE12FDFD3"
      org-crypt-disable-auto-save t)

(let ((map org-mode-map))
  (define-key map (kbd "C-c o e") #'org-encrypt-entry)
  (define-key map (kbd "C-c o d") #'org-decrypt-entry)
  (define-key map (kbd "C-c o E") #'org-encrypt-entries)
  (define-key map (kbd "C-c o D") #'org-decrypt-entries))

(provide 'init-org-crypt)
;;; init-org-crypt.el ends here
