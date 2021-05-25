;;; init-nix.el --- Init configuration

;;; Commentary:

;; This contains init configuration for Nix and NixOS.

;;; Code:

(require 'nix-mode)

(add-to-list 'auto-mode-alist  '("\\.nix\\'" . nix-mode))

(defun my-find-nixos-config-file ()
  (interactive)
  (find-file "/home/nas/nix/configuration.nix"))

(defun my-find-nixos-home-file ()
  (interactive)
  (find-file "/home/nas/.config/nixpkgs/home.nix"))

(defvar nixos-helpers-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" #'my-find-nixos-config-file)
    (define-key map "h" #'my-find-nixos-home-file)
    map)
  "Key map for my NixOS helper functions.")

(global-set-key (kbd "C-c n") nixos-helpers-map)

(defun nix-mode-my-config ()
  (add-to-list (make-local-variable 'company-backends) 'company-nixos-options))

(add-hook 'nix-mode-hook #'nix-mode-my-config)

(provide 'init-nix)
;;; init-nix.el ends here
