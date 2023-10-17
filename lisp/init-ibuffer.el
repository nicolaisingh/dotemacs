;;; init-ibuffer.el --- Init customization

;;; Commentary:

;; This contains init customization for the package ibuffer.

;;; Code:

(require 'ibuffer)
(require 'ibuf-ext)

(setq ibuffer-use-other-window t
      ibuffer-show-empty-filter-groups nil
      ibuffer-show-empty-filter-groups nil
      ibuffer-saved-filter-groups
      '(("nas"
         ("Emacs Lisp" (mode . emacs-lisp-mode))
         ("Javascript" (mode . js2-mode))
         ("Shell/Term" (or (mode . shell-mode)
                           (mode . eshell-mode)
                           (mode . term-mode)))
         ("Kotlin" (mode . kotlin-mode))
         ("Nix" (mode . nix-mode))
         ("Org" (mode . org-mode))
         ("Dired" (mode . dired-mode))
         ("Magit" (name . "^magit.*:"))
         ("xref" (name . "^\\*xref\\*$"))
         ("emacs" (or
                   (name . "^\\*Customize\\*$")
                   (name . "^\\*scratch\\*$")
                   (name . "^\\*Messages\\*$")
                   (name . "^\\*Backtrace\\*$")
                   (name . "^\\*Help\\*$")
                   (name . "^\\*RE-Builder\\*$")
                   (name . "^\\*Async-native-compile-log\\*$")
                   (name . "^\\*Packages\\*$"))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "nas")))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
