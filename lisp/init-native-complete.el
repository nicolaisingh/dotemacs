;;; init-native-complete.el --- Init customization

;;; Commentary:

;; This contains init customization for the package native-complete.

;;; Code:

(with-eval-after-load 'shell
  (native-complete-setup-bash))

;; NOTE: For the company-mode completion backend,
;; company-native-complete is set up in init-company

(provide 'init-native-complete)
;; init-native-complete.el ends here
