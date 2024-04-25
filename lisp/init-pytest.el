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

(keymap-set python-mode-map "C-c P ." #'pytest-one)
(keymap-set python-mode-map "C-c P !" #'pytest-again)
(keymap-set python-mode-map "C-c P m" #'pytest-module)
(keymap-set python-mode-map "C-c P a" #'pytest-all)
(keymap-set python-mode-map "C-c C ." #'pytest-cov-one)
(keymap-set python-mode-map "C-c C !" #'pytest-cov-again)
(keymap-set python-mode-map "C-c C m" #'pytest-cov-module)
(keymap-set python-mode-map "C-c C a" #'pytest-cov-all)

(provide 'init-pytest)
;;; init-pytest.el ends here
