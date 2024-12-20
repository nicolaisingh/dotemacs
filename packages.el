;; Bootstrap
(load (expand-file-name "lisp/straight/bootstrap.el" user-emacs-directory) nil t)

;; Load packages
(setq straight-packages
      '((org :type built-in)
        ;; for json-navigator
        (hierarchy :type built-in)

        adoc-mode
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
        consult-org-roam
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
        (howm :host github :repo "kaorahi/howm" :files (:defaults (:exclude "*.el.in")) :fork t)
        htmlize
        imenu-list
        inf-clojure
        js2-mode
        js2-refactor
        json-mode
        json-navigator
        jsonian
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
        mermaid-mode
        mermaid-ts-mode
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
        org-alert
        org-bookmark-heading
        org-modern
        org-present
        org-roam
        org-sticky-header
        org-web-tools
        orgalist
        origami
        osm
        ox-asciidoc
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
        prettier-js
        prism
        prodigy
        pytest
        qrencode
        readline-complete
        restclient
        (saveplace-pdf-view :host github :depth full)
        selected
        shell-maker
        slime
        smartparens
        solidity-mode
        tide
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
