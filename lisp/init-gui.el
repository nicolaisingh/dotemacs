;;; init-gui.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the Emacs GUI.

;;; Code:

(setq initial-scratch-message "")
(setq initial-major-mode 'fundamental-mode)
(save-place-mode 1)

;; Most of the code here are now in early-init.el to utilize the early
;; init mechanism introduced in Emacs 27
(when (< emacs-major-version 27)
  (let ((early-init-file (expand-file-name "early-init.el" user-emacs-directory)))
    (when (file-exists-p early-init-file)
      (load early-init-file))))

(provide 'init-gui)
;;; init-gui.el ends here
