;;; init-plantuml.el --- Init configuration

;;; Commentary:

;; This contains init configuration for anything PlantUML-related.

;;; Code:

(setq plantuml-jar-path "/usr/share/java/plantuml.jar"
      plantuml-output-type "png")

(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

(provide 'init-plantuml)
;;; init-plantuml.el ends here
