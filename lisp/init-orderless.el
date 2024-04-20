;;; init-orderless.el --- Init customization

;;; Commentary:

;; This contains init customization for orderless.

;;; Code:

(require 'orderless)

(setq completion-styles '(orderless basic)
      ;; completion-styles '(flex partial-completion basic)
      ;; Completion falls back to using completion-styles if
      ;; completion-category-overrides doesn't yield a
      ;; result
      completion-category-overrides '((file (styles (basic flex partial-completion)))
                                      (buffer (styles (basic flex partial-completion)))))

(provide 'init-orderless)
;;; init-orderless.el ends here
