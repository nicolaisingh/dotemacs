;;; init-isearch.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package isearch.

;;; Code:

(require 'isearch)

;; Do not exit incremental search when scrolling
(setq-default isearch-allow-scroll 1)

;; Make space characters match anything
(setq search-whitespace-regexp ".+?")

;; Show match numbers in the search prompt
(setq isearch-lazy-count t)

;; Straight up cancels isearch without the rub out behavior.
(defun my-isearch-control-g ()
  (interactive)
  (setq isearch-success nil)
  (isearch-cancel))

(defun isearch-mode-my-custom-keys ()
  (define-key isearch-mode-map (kbd "C-g") #'my-isearch-control-g))

(add-hook 'isearch-mode-hook #'isearch-mode-my-custom-keys)

(provide 'init-isearch)
;;; init-isearch.el ends here
