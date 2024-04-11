;;; init-pytest.el --- Init customization

;;; Commentary:

;; This contains init customization for pytest.

;;; Code:

(require 'python)
(require 'pytest)

(defun pytest-cov-one ()
  (interactive)
  (let ((pytest-global-name "coverage run -m pytest"))
    (pytest-one)))

(defun pytest-cov-again ()
  (interactive)
  (let ((pytest-global-name "coverage run -m pytest"))
    (pytest-again)))

(defun pytest-cov-module ()
  (interactive)
  (let ((pytest-global-name "coverage run -m pytest"))
    (pytest-module)))

(defun pytest-cov-all ()
  (interactive)
  (let ((pytest-global-name "coverage run -m pytest"))
    (pytest-all)))

(define-key python-mode-map (kbd "C-c P .") #'pytest-one)
(define-key python-mode-map (kbd "C-c P !") #'pytest-again)
(define-key python-mode-map (kbd "C-c P m") #'pytest-module)
(define-key python-mode-map (kbd "C-c P a") #'pytest-all)
(define-key python-mode-map (kbd "C-c C .") #'pytest-cov-one)
(define-key python-mode-map (kbd "C-c C !") #'pytest-cov-again)
(define-key python-mode-map (kbd "C-c C m") #'pytest-cov-module)
(define-key python-mode-map (kbd "C-c C a") #'pytest-cov-all)

(provide 'init-pytest)
;;; init-pytest.el ends here
