;;; init-isearch.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package isearch.

;;; Code:

(require 'isearch)

;; Do not exit incremental search when scrolling
(setq-default isearch-allow-scroll 1)

;; Make space characters match anything
(setq search-whitespace-regexp ".+?")

;; Straight up cancels isearch without the rub out behavior.
(defun my-isearch-control-g ()
  (interactive)
  (setq isearch-success nil)
  (isearch-cancel))

;; Show match numbers in the search prompt
(setq isearch-lazy-count t)

(define-key isearch-mode-map (kbd "C-g") #'my-isearch-control-g)

(provide 'init-isearch)
;;; init-isearch.el ends here
