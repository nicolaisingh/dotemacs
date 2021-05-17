;;; init-nix.el --- Init configuration

;;; Commentary:

;; This contains init configuration for Nix and NixOS.

;;; Code:

(require 'nix-mode)

(add-to-list 'auto-mode-alist  '("\\.nix\\'" . nix-mode))

(defun my-find-nixos-config-file ()
  (interactive)
  (sudo-find-file "/etc/nixos/configuration.nix"))

(global-set-key (kbd "C-c n c") #'my-find-nixos-config-file)

(provide 'init-nix)
;;; init-nix.el ends here
