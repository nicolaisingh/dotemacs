;;; init-javascript.el --- Init customization

;;; Commentary:

;; This contains init customization for programming in Javascript.

;;; Code

(require 'init-etc)
(require 'js2-mode)
(require 'js2-refactor)
(require 'xref-js2)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(setq js-indent-level 2
      js-switch-indent-offset 2
      js2-strict-missing-semi-warning nil)

(defun js2-mode-my-custom-keys ()
  (keymap-set js2-mode-map "C-M-SPC" #'js2-mark-parent-statement)
  (keymap-set js2-mode-map "C-M-h" #'js2-mark-defun)
  (keymap-set js2-mode-map "C-c m j" #'js2-print-json-path)
  (keymap-set js2-mode-map "<return>" #'js2-line-break)
  (keymap-set js2-mode-map "C-k" #'js2r-kill)

  ;; Let xref-js2 handle references and definitions
  (keymap-unset js2-mode-map "M-."))

(defun json-mode-my-custom-keys ()
  (keymap-set json-mode-map "C-c m j" #'jsons-print-path))

(add-hook 'js2-mode-hook #'indent-spaces)
(add-hook 'js2-mode-hook #'js2-mode-my-custom-keys)
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(add-hook 'js2-mode-hook #'subword-mode)
(add-hook 'json-mode-hook #'json-mode-my-custom-keys)

;; js2-refactor
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-m")

;; xref-js2 (M-. M-? M-,)
(defun hook-xref-js2-backend ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend))
(add-hook 'js2-mode-hook #'hook-xref-js2-backend)


(provide 'init-javascript)
;;; init-javascript.el ends here
