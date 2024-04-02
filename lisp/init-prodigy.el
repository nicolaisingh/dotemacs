;;; init-prodigy.el --- Init customization

;;; Commentary:

;; This contains init customization for prodigy.

;;; Code:

(require 'prodigy)
(global-set-key (kbd "C-c P p") #'prodigy)

(setq prodigy-services '((:name "proton-bridge"
                                :command "protonmail-bridge"
                                :args ("-n")
                                :tags (proton)))
      prodigy-tags '((:name proton
                            :stop-signal int
                            :kill-process-buffer-on-stop t)))

(provide 'init-prodigy)
;;; init-prodigy.el ends here
