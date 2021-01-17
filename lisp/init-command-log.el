;;; init-command-log.el --- Init configuration

;;; Commentary:

;; This contains init configuration for command-log-mode.

;;; Code:

(setq command-log-mode-window-font-size 0
      command-log-mode-window-size 60
      command-log-mode-key-binding-open-log "C-c c l"
      command-log-mode-open-log-turns-on-mode t)

(require 'command-log-mode)

;; The original function definition hard-coded value when setting the
;; text scale, ignoring `command-log-mode-window-font-size'.
(defun clm/open-command-log-buffer (&optional arg)
  "Opens (and creates, if non-existant) a buffer used for logging keyboard commands.
If ARG is Non-nil, the existing command log buffer is cleared."
  (interactive "P")
  (with-current-buffer
      (setq clm/command-log-buffer
            (get-buffer-create " *command-log*"))
    (text-scale-set command-log-mode-window-font-size))
  (when arg
    (with-current-buffer clm/command-log-buffer
      (erase-buffer)))
  (let ((new-win (split-window-horizontally
                  (- 0 command-log-mode-window-size))))
    (set-window-buffer new-win clm/command-log-buffer)
    (set-window-dedicated-p new-win t)))

(provide 'init-command-log)
;;; init-command-log.el ends here
