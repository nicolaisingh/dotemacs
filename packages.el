;; Bootstrap
(load (expand-file-name "lisp/straight/bootstrap.el" user-emacs-directory) nil t)

;; Load packages
(setq straight-packages
      '((org :type built-in)
        ;; for json-navigator
        (hierarchy :type built-in)

        ag
        aggressive-indent
        alert
        amx
        atomic-chrome
        avy
        blacken
        browse-kill-ring
        bui
        calibre
        cape
        centered-cursor-mode
        chatgpt-shell
        chess
        chronos
        clojure-mode
        command-log-mode
        company
        company-go
        company-native-complete
        company-nixos-options
        company-restclient
        consult
        consult-dir
        corfu
        cov
        csv-mode
        currency-convert
        dap-mode
        dash
        deadgrep
        devdocs
        diff-hl
        diminish
        dired-recent
        dired-toggle
        dockerfile-mode
        easy-kill
        edit-indirect
        edit-server
        elpy
        emacsql
        emms
        esh-help
        eshell-up
        eshell-z
        exec-path-from-shell
        expand-region
        f
        find-file-in-repository
        flycheck
        flycheck-kotlin
        flycheck-package
        focus
        ;; git-commit
        git-timemachine
        go-mode
        gptel
        graphviz-dot-mode
        highlight-indent-guides
        highlight-numbers
        htmlize
        imenu-list
        inf-clojure
        js2-mode
        js2-refactor
        json-mode
        json-navigator
        know-your-http-well
        kotlin-mode
        log4e
        lorem-ipsum
        lsp-docker
        lsp-mode
        lsp-treemacs
        magit
        magit-section
        marginalia
        markdown-mode
        multi-term
        multiple-cursors
        native-complete
        nav-flash
        nix-mode
        nixos-options
        nov
        orderless
        org-alert
        org-modern
        org-present
        org-roam
        origami
        osm
        ox-gfm
        ox-jira
        ox-slack
        package-lint
        pass
        password-store
        password-store-otp
        pcsv
        pdf-tools
        plantuml-mode
        prism
        prodigy
        pytest
        readline-complete
        restclient
        (saveplace-pdf-view
         :type git :flavor melpa :host github :repo "nicolaisingh/saveplace-pdf-view" :depth full)
        selected
        shell-maker
        slime
        smartparens
        solidity-mode
        transient
        transpose-frame
        tree-mode
        treemacs
        typescript-mode
        typing
        unfill
        uuidgen
        visual-fill-column
        which-key
        with-editor
        writeroom-mode
        xref-js2
        yaml
        yaml-imenu
        yaml-mode
        yasnippet
        ztree))

(dolist (package straight-packages)
  (straight-use-package package))
