;;; init-chatgpt-shell.el --- Init customization

;;; Commentary:

;; This contains init customization for chatgpt-shell.

;;; Code:

(require 'chatgpt-shell)

(add-to-list 'chatgpt-shell-system-prompts
             `("Concise" . ,(string-join '("I need you to reply as concise and direct to the point as possible."
                                           "Please format all responses in org-mode"
                                           "Don't wrap responses in markdown.") "  ")) t)

(setq chatgpt-shell-openai-key (lambda ()
                                 (auth-source-pick-first-password :host "api.openai.com"))
      chatgpt-shell-prompt-query-response-style 'shell
      chatgpt-shell-welcome-function nil
      chatgpt-shell-system-prompt (seq-position (map-keys chatgpt-shell-system-prompts) "Concise"))

(defun my-chatgpt-shell-rephrase-sentences ()
  (interactive)
  (chatgpt-shell-send-region-with-header
   (string-join
    '("I need you to improve and rephrase sentences without sounding too formal."
      "Give me only 3 improvements for each sentence, nothing else.") "  ")))

(defun my-chatgpt-shell-show-prompt ()
  (interactive)
  (let ((message-log-max nil))
    (if (not chatgpt-shell-system-prompt)
        (message "No prompt selected.")
      (message "%s" (cdr (nth chatgpt-shell-system-prompt
                              chatgpt-shell-system-prompts))))))

(defun my-chatgpt-shell-prompt ()
  (interactive)
  (let ((chatgpt-shell-prompt-query-response-style 'inline))
    (call-interactively #'chatgpt-shell-prompt)))

(keymap-set chatgpt-shell-mode-map "C-c C-S-p" #'chatgpt-shell-load-awesome-prompts)
(keymap-set chatgpt-shell-mode-map "C-c C-S-s" #'my-chatgpt-shell-show-prompt)

(keymap-global-set "C-c l l" #'chatgpt-shell)
(keymap-global-set "C-c l m" #'my-chatgpt-shell-prompt)
(keymap-global-set "C-c l x" #'chatgpt-shell-explain-code)
(keymap-global-set "C-c l r" #'chatgpt-shell-refactor-code)
(keymap-global-set "C-c l u" #'chatgpt-shell-generate-unit-test)
(keymap-global-set "C-c l p" #'chatgpt-shell-proofread-region)
(keymap-global-set "C-c l SPC" #'chatgpt-shell-send-region)
(keymap-global-set "C-c l C-SPC" #'chatgpt-shell-send-and-review-region)

(provide 'init-chatgpt-shell)
;;; init-chatgpt-shell.el ends here
