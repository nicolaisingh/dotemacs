;;; init-org-alert.el --- Init customization

;;; Commentary:

;; This contains init customization for org-alert.

;;; Code:

(require 'alert)
(require 'org-alert)

(setq org-alert-interval 300
      org-alert-notification-title "Org alert"
      org-alert-notify-cutoff 0
      org-alert-notify-after-event-cutoff 30)

(alert-add-rule :title org-alert-notification-title
                :style 'libnotify
                :continue t
                :persistent t)

(alert-add-rule :title org-alert-notification-title
                :style 'org-alert-email
                :continue t)

(alert-add-rule :title org-alert-notification-title
                :style 'legacy-log
                :continue t)

(org-alert-enable)

(provide 'init-org-alert)
;;; init-org-alert.el ends here
