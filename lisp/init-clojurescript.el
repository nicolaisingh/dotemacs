;;; init-clojurescript.el --- Init configuration

;;; Commentary:

;; This contains init configuration for programming in ClojureScript.

;;; Code:

(setq inf-clojure-custom-repl-type 'cljs)
(setq inf-clojure-custom-startup "clojure -m cljs.main -r")

(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
(add-hook 'clojurescript-mode-hook #'inf-clojure-minor-mode)

(provide 'init-clojurescript)
;;; init-clojurescript.el ends here
