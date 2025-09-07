(define-derived-mode tl-playground-mode tabulated-list-mode "TabulatedList"
  (setq tabulated-list-format [("First" 10 t)
                               ("Second" 10 t)
                               ("Third" 20 nil)
                               ("Fourth" 20 t)])
  (tabulated-list-init-header))

(defun tl-playground-list-entries ()
  '(((1 2 3) ["One" "Two" "Three" "aaaaa"])
    ((4 5 6) ["Four" "Five" "Six" "bbbbb"])))

(defun list-tl-playground ()
  (interactive)
  (let ((buffer (get-buffer-create "*tabulated-list-playground*")))
    (with-current-buffer buffer
      (setq tabulated-list-entries #'tl-playground-list-entries
            tabulated-list-sort-key '("Second" . nil))
      (unless (eq major-mode 'tl-playground-mode)
        (tl-playground-mode))
      (tabulated-list-print))
    (pop-to-buffer buffer)))
