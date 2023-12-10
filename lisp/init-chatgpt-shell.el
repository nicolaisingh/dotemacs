;;; init-chatgpt-shell.el --- Init customization

;;; Commentary:

;; This contains init customization for chatgpt-shell.

;;; Code:

(require 'chatgpt-shell)

(setq chatgpt-shell-openai-key
      (lambda ()
        (auth-source-pick-first-password :host "api.openai.com")))

(add-to-list 'chatgpt-shell-model-versions "gpt-3.5-turbo-1106")

(provide 'init-chatgpt-shell)
;;; init-chatgpt-shell.el ends here
