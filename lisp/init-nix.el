;;; init-nix.el --- Init configuration

;;; Commentary:

;; This contains init configuration for Nix and NixOS.

;;; Code:

(require 'nix-mode)

(add-to-list 'auto-mode-alist  '("\\.nix\\'" . nix-mode))

(defun my-find-nixos-config-file ()
  (interactive)
  (find-file "/home/nas/nix/configuration.nix"))

(defvar nixos-helpers-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "c" #'my-find-nixos-config-file)
    map)
  "Key map for my NixOS helper functions.")

(keymap-global-set "C-c n" nixos-helpers-map)

(add-hook 'nix-mode-hook #'smartparens-mode)

(provide 'init-nix)
;;; init-nix.el ends here
