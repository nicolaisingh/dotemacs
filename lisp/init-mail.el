;;; init-mail.el --- Init customization

;;; Commentary:

;; This contains init customization for sending mail.

;;; Code:

(setq smtpmail-smtp-server "127.0.0.1"
      smtpmail-smtp-service 1025
      smtpmail-stream-type 'starttls
      ;; smtpmail-servers-requiring-authorization "127.0.0.1"
      send-mail-function 'sendmail-send-it
      sendmail-program "msmtp")

(provide 'init-mail)
;;; init-mail.el ends here
