;;; init-yasnippet.el --- Init customization

;;; Commentary:

;; This contains init customization for yasnippet.

;;; Code:

(require 'yasnippet)

(setq yas-minor-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "s s") 'yas-expand)
        (define-key map (kbd "s i") 'yas-insert-snippet)
        (define-key map (kbd "s n") 'yas-new-snippet)
        (define-key map (kbd "s v") 'yas-visit-snippet-file)
        (define-key map (kbd "s t") 'yas-tryout-snippet)
        (define-key map (kbd "s d") 'yas-describe-tables)
        (define-key map (kbd "s r") 'yas-reload-all)
        map)
      yas-prompt-functions '(yas-completing-prompt yas-no-prompt)
      yas-wrap-around-region t)

(define-key global-map (kbd "C-c y") yas-minor-mode-map)

(set-face-attribute 'yas-field-highlight-face t
                    :inherit 'minibuffer-prompt)

(yas-global-mode t)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
