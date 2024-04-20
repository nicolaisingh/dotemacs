;;; init-corfu-cape.el --- Init customization

;;; Commentary:

;; This contains init customization for corfu and cape.

;;; Code:

(require 'corfu)
(require 'corfu-echo)
(require 'cape)

(setq corfu-auto nil
      corfu-auto-delay 0.2
      corfu-auto-prefix 3
      corfu-echo-delay '(1.0 . 0.5)
      tab-always-indent 'complete)

(keymap-set corfu-map "C-s" #'corfu-next)
(keymap-set corfu-map "C-." #'corfu-next)
(keymap-set corfu-map "C-r" #'corfu-previous)
(keymap-set corfu-map "C-," #'corfu-previous)

(global-corfu-mode)
(corfu-echo-mode t)

;; (keymap-global-set "C-c p p" #'completion-at-point) ;; capf
;; (keymap-global-set "C-c p t" #'complete-tag)        ;; etags
(keymap-global-set "C-c p d" #'cape-dabbrev)
(keymap-global-set "C-c p h" #'cape-history)
(keymap-global-set "C-c p f" #'cape-file)
(keymap-global-set "C-c p k" #'cape-keyword)
(keymap-global-set "C-c p s" #'cape-elisp-symbol)
(keymap-global-set "C-c p e" #'cape-elisp-block)
(keymap-global-set "C-c p a" #'cape-abbrev)
(keymap-global-set "C-c p l" #'cape-line)
(keymap-global-set "C-c p w" #'cape-dict)
(keymap-global-set "C-c p :" #'cape-emoji)
(keymap-global-set "C-c p \\" #'cape-tex)
;; (keymap-global-set "C-c p _" #'cape-tex)
;; (keymap-global-set "C-c p ^" #'cape-tex)
(keymap-global-set "C-c p &" #'cape-sgml)
(keymap-global-set "C-c p r" #'cape-rfc1345)

(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-line)
(add-to-list 'completion-at-point-functions #'cape-elisp-block)

(provide 'init-corfu-cape)
;;; init-corfu-cape.el ends here
