;;; early-init.el --- Early init configuration -*- lexical-binding: t -*-

;;; Commentary:

;; This contains early-init configuration.

;;; Code:

(setq blink-cursor-blinks 0
      blink-cursor-interval 0.2
      blink-cursor-delay 0.2
      cursor-type t
      inhibit-startup-screen t
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
