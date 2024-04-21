;;; init-highlight-indent-guides.el --- Init customization

;;; Commentary:

;; This contains init customization for highlight-indent-guides.

;;; Code:

(require 'highlight-indent-guides)

(setq highlight-indent-guides-method 'bitmap
      highlight-indent-guides-bitmap-function #'highlight-indent-guides--bitmap-dots
      highlight-indent-guides-responsive nil
      highlight-indent-guides-delay 0
      highlight-indent-guides-auto-character-face-perc 20
      highlight-indent-guides-auto-top-character-face-perc 40
      highlight-indent-guides-auto-stack-character-face-perc 30)

(keymap-global-set "C-c h i" #'highlight-indent-guides-mode)

(provide 'init-highlight-indent-guides)
;;; init-highlight-indent-guides.el ends here
