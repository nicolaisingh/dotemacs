(require 'company)
(setq company-lighter-base "Comp"
      company-minimum-prefix-length 2
      company-idle-delay 0.3
      company-show-quick-access 'left
      company-selection-wrap-around 1
      company-require-match nil
      company-dabbrev-minimum-length 3
      company-dabbrev-downcase nil
      company-transformers '(company-sort-by-occurrence
                             company-sort-by-backend-importance)
      company-global-modes '(not eshell-mode term-mode))
(add-to-list 'company-backends 'company-native-complete)
(add-to-list 'company-backends 'company-go)
(add-to-list 'company-backends 'company-restclient)
;; This backend sometimes produces an error when in other modes, so
;; enable only on nix-mode
(with-eval-after-load 'nix-mode
  (add-hook 'nix-mode-hook (lambda ()
                             (interactive)
                             (make-local-variable 'company-backends)
                             (add-to-list 'company-backends 'company-nixos-options))))
(keymap-set company-active-map "C-n" #'company-select-next)
(keymap-set company-active-map "C-p" #'company-select-previous)
(keymap-set company-active-map "C-c C-/" #'company-other-backend)
(add-hook 'after-init-hook #'global-company-mode)
