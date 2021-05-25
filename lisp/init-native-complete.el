;;; init-native-complete.el --- Init customization

;;; Commentary:

;; This contains init customization for the package native-complete.
;;
;; Some notes:
;; - For the company-mode completion backend, company-native-complete
;; is set up in init-company
;; - Put HISTCONTROL=ignoreboth in bashrc to avoid polluting the shell
;; history with the `echo' commands made by this package

;;; Code:

(with-eval-after-load 'shell
  (require 'native-complete)
  (native-complete-setup-bash))

(provide 'init-native-complete)
;; init-native-complete.el ends here
