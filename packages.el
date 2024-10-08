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
        atomic-chrome
        avy
        blacken
        browse-kill-ring
        calibre
        cape
        centered-cursor-mode
        chatgpt-shell
        chess
        chronos
        clojure-mode
        command-log-mode
        consult
        consult-dir
        corfu
        cov
        csv-mode
        currency-convert
        dap-mode
        dart-mode
        dash
        deadgrep
        devdocs
        diff-hl
        diminish
        dired-narrow
        dired-recent

        dired-subtree
        dired-sidebar

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
        flycheck
        flycheck-kotlin
        flycheck-package
        focus
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
        marginalia
        markdown-mode
        multi-term
        multiple-cursors
        native-complete
        nav-flash
        nix-mode
        nixos-options
        nov
        ob-chatgpt-shell
        ob-kotlin
        ob-nix
        orderless
        org-bookmark-heading
        org-alert
        org-modern
        org-present
        org-roam
        org-sticky-header
        origami
        osm
        ox-gfm
        ox-jira
        ox-slack
        package-lint

        ;; pass deps
        (password-store-otp
         :host github :repo "volrath/password-store-otp.el" :fork t)
        password-store
        pass

        pcsv
        pdf-tools
        plantuml-mode
        prism
        prodigy
        pytest
        readline-complete
        restclient
        (saveplace-pdf-view :host github :depth full)
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
