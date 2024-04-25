;;; init-yasnippet.el --- Init customization

;;; Commentary:

;; This contains init customization for yasnippet.

;;; Code:

(require 'yasnippet)

(setq yas-minor-mode-map
      (let ((map (make-sparse-keymap)))
        (keymap-set map "s" 'yas-expand)
        (keymap-set map "i" 'yas-insert-snippet)
        (keymap-set map "n" 'yas-new-snippet)
        (keymap-set map "v" 'yas-visit-snippet-file)
        (keymap-set map "t" 'yas-tryout-snippet)
        (keymap-set map "d" 'yas-describe-tables)
        (keymap-set map "r" 'yas-reload-all)
        map)
      yas-prompt-functions '(yas-completing-prompt yas-no-prompt)
      yas-wrap-around-region t)

(keymap-global-set "C-c y s" yas-minor-mode-map)

(set-face-attribute 'yas-field-highlight-face t
                    :inherit 'minibuffer-prompt)

(yas-global-mode t)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
