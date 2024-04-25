;;; init-nix.el --- Init configuration

;;; Commentary:

;; This contains init configuration for Nix and NixOS.

;;; Code:

(require 'nix-mode)

(add-to-list 'auto-mode-alist  '("\\.nix\\'" . nix-mode))

(defun my-find-nixos-config-file ()
  (interactive)
  (find-file "/home/nas/nix/configuration.nix"))

(add-hook 'nix-mode-hook #'smartparens-mode)

(provide 'init-nix)
;;; init-nix.el ends here
