;;; early-init.el --- Early init configuration -*- lexical-binding: t -*-

;;; Commentary:

;; This contains early-init configuration.

;;; Code:

(setq blink-cursor-blinks 0)
(setq blink-cursor-interval 0.2)
(setq blink-cursor-delay 0.2)
(setq cursor-type t)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(setq visible-bell t)
(blink-cursor-mode)
(column-number-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Make the fringe slightly thicker
;; (add-to-list 'fringe-styles '("padded" 12))
;; (fringe-mode 12)

;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(width . 240))
(add-to-list 'default-frame-alist '(height . 90))

(provide 'early-init)
;;; early-init.el ends here
