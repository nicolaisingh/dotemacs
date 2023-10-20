;;; init-chronos.el --- Init customization

;;; Commentary:

;; This contains init customization for chronos.

;;; Code:

(require 'chronos)

(setq chronos-standard-timers '("0:0:30/30-second finished"
                                "5/5-minute timer finished"
                                "15/15-minute timer finished"
                                "30/30-minutes timer finished"
                                "01:00/1-hour timer finished")
      chronos-expiry-functions '(chronos-message-notify
                                 chronos-desktop-notifications-notify))

(global-set-key (kbd "C-c T") #'chronos-initialize)

(provide 'init-chronos)
;;; init-chronos.el ends here
