;;; init-isearch.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package isearch.

;;; Code:

(require 'isearch)

(setq
 ;; Do not exit incremental search when scrolling
 isearch-allow-scroll 'unlimited

 ;; Make space characters match anything
 search-whitespace-regexp ".+?"

 ;; Show match numbers in the search prompt
 isearch-lazy-count t)

;; Straight up cancels isearch without the rub out behavior.
(defun my-isearch-control-g ()
  (interactive)
  (setq isearch-success nil)
  (isearch-cancel))

(defun isearch-mode-my-custom-keys ()
  (define-key isearch-mode-map (kbd "C-g") #'my-isearch-control-g)
  (define-key isearch-mode-map (kbd "C-o") #'other-window))

(add-hook 'isearch-mode-hook #'isearch-mode-my-custom-keys)

(provide 'init-isearch)
;;; init-isearch.el ends here
