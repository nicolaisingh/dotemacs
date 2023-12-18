;;; init-chronos.el --- Init customization

;;; Commentary:

;; This contains init customization for chronos.

;;; Code:

(require 'chronos)

(defun chronos-osascript-notify (c)
  "(For Mac OS) Notify expiration of timer C using osascript."
  (let ((shell-command-string (concat "osascript -e "
                                      "'"
                                      "display notification "
                                      "\"" (chronos--time-string c) ": " (chronos--message c) "\""
                                      "with title "
                                      "\"Emacs\""
                                      "'")))
    (shell-command shell-command-string)))

(defun chronos-alert (c)
  (alert (chronos--message c)
         :title (concat (chronos--time-string c) ": Timer expired")
         :style 'osx-notifier))

(setq chronos-standard-timers '("0:0:30/30-second finished"
                                "5/5-minute timer finished"
                                "15/15-minute timer finished"
                                "30/30-minutes timer finished"
                                "01:00/1-hour timer finished")
      chronos-expiry-functions '(chronos-message-notify
                                 ;; For macOS: Use chronos-alert instead of chronos-desktop-notifications-notify
                                 chronos-desktop-notifications-notify))

(defun chronos-load ()
  (interactive)
  (if (get-buffer chronos-buffer-name)
      (switch-to-buffer chronos-buffer-name)
    (chronos-initialize)))

(global-set-key (kbd "C-c T") #'chronos-load)

(provide 'init-chronos)
;;; init-chronos.el ends here
