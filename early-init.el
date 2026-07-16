;;; early-init.el --- Early init configuration -*- lexical-binding: t -*-

;;; Commentary:

;; This contains early-init configuration.

;;; Code:

;; Handling for custom flag --profile
;; Read the flag eagerly from `command-line-args' so that the value is
;; available while init.el loads; the `command-switch-alist' handler below
;; runs only afterwards and merely consumes the switch's argument.
(defvar my-emacs-profile
  (cadr (member "--profile" command-line-args))
  "Startup profile passed via the `--profile' command-line flag.")

(push '("--profile" . (lambda (_arg)
                        (message "Emacs Profile: %s" (pop command-line-args-left))))
      command-switch-alist)

(setq
 ;; Cursor settings
 blink-cursor-blinks 0
 blink-cursor-interval 0.2
 blink-cursor-delay 0.2
 cursor-type t

 ;; Use elpaca
 package-enable-at-startup nil

 ring-bell-function 'ignore
 visible-bell t)

(blink-cursor-mode 1)
(column-number-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Make the fringe slightly thicker
(fringe-mode 12)

;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(width . 240))
(add-to-list 'default-frame-alist '(height . 70))

(provide 'early-init)
;;; early-init.el ends here
