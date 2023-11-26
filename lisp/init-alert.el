;;; init-alert.el --- Init customization

;;; Commentary:

;; This contains init customization for alert.

;;; Code:

(require 'alert)

(setq alert-fade-time 10)

(alert-define-style 'org-alert-email
                    :title "org-alert email"
                    :notifier
                    (lambda (info)
                      (save-window-excursion
                        (let ((title (concat "["
                                             (plist-get info :title)
                                             "] "
                                             (file-name-base (buffer-name (plist-get info :buffer)))))
                              (body (plist-get info :message))
                              (message-interactive nil))
                          (message-mail user-mail-address title)
                          (message-goto-body)
                          (insert body)
                          (insert (format "\n\n-----\nAlert time: %s"
                                          (format-time-string "%Y-%m-%d %H:%M:%S")))
                          (message-send-and-exit)))))

(alert-define-style 'legacy-log
                    :title "Log to *Alerts* buffer (legacy)"
                    :notifier
                    (lambda (info)
                      (let* ((mes (plist-get info :message))
                             (sev (plist-get info :severity))
                             (len (length mes)))
                        (alert-legacy-log-notify mes sev len))))

(provide 'init-alert)
;;; init-alert.el ends here
