;;; init-org-alert.el --- Init customization

;;; Commentary:

;; This contains init customization for org-alert.

;;; Code:

(require 'alert)
(require 'org-alert)

(setq org-alert-interval 60
      org-alert-notification-title "Org Alert"
      org-alert-notify-cutoff 0)

(alert-add-rule :title org-alert-notification-title
                :style 'libnotify
                :continue t
                :persistent t)

(org-alert-enable)

(provide 'init-org-alert)
;;; init-org-alert.el ends here
