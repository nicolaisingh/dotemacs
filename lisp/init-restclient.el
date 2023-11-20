;;; init-restclient.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package restclient.

;;; Code:

(require 'restclient)

(add-to-list 'auto-mode-alist '("\\.restclient\\'" . restclient-mode))
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

(defun restclient-my-config ()
  (setq tab-width 2
        restclient-same-buffer-response nil
        restclient-response-size-threshold nil)
  (smartparens-strict-mode 1)
  (indent-tabs-mode -1))

(defun restclient-no-gc-before-request ()
  (increase-gc-cons-percentage))

(defun restclient-gc-after-response ()
  (revert-gc-cons-percentage)
  (garbage-collect))

(defun restclient-format-response ()
  (show-paren-local-mode -1)
  (font-lock-mode -1)
  (buffer-disable-undo)
  (buffer-enable-undo))


(add-hook 'restclient-mode-hook #'restclient-my-config)
;; (add-hook 'restclient-http-do-hook #'restclient-no-gc-before-request)
;; (add-hook 'restclient-response-loaded-hook #'restclient-gc-after-response)
;; (add-hook 'restclient-response-loaded-hook #'restclient-format-response)

(provide 'init-restclient)
;;; init-restclient.el ends here
