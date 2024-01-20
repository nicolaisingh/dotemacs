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
                        (let ((tag (concat "["
                                           (plist-get info :title)
                                           "] "))
                              (body (plist-get info :message))
                              (title-body-length 40)
                              (message-interactive nil))
                          (message-mail "nicolaisingh+org-alert@protonmail.com"
                                        (concat tag (if (length> body title-body-length)
                                                        (concat (substring body 0 title-body-length) "...")
                                                      body)))
                          (message-goto-body)
                          (insert (format "[From %s]\n"
                                          (file-name-base (buffer-name (plist-get info :buffer)))))
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
