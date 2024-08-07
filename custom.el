(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completions-format 'vertical)
 '(dictionary-server "dict.org")
 '(elpy-modules
   '(elpy-module-company elpy-module-eldoc elpy-module-flymake
                         elpy-module-pyvenv elpy-module-django
                         elpy-module-sane-defaults))
 '(ibuffer-saved-filter-groups
   '(("nas" ("Dired" (mode . dired-mode))
      ("Emacs Lisp" (mode . emacs-lisp-mode))
      ("Kotlin" (mode . kotlin-mode)) ("Magit" (name . "^magit.*:"))
      ("Org" (mode . org-mode)) ("Nix" (mode . nix-mode))
      ("Shell" (mode . shell-mode)) ("xref" (name . "^\\*xref\\*$")))))
 '(ibuffer-show-empty-filter-groups nil)
 '(kotlin-tab-width 4)
 '(mode-line-percent-position nil)
 '(newsticker-date-format "(%A, %H:%M, %b %d %Y)")
 '(newsticker-frontend 'newsticker-plainview)
 '(newsticker-item-format "* %t")
 '(newsticker-retrieval-interval 86400)
 '(newsticker-scroll-smoothly t)
 '(newsticker-statistics-format "[new-%n imm-%i old-%o obs-%O tot-%a]")
 '(newsticker-url-list
   '(("Planet EmacsLife" "https://planet.emacslife.com/atom.xml" nil nil
      nil)
     ("Mattia Gheda" "http://ghedam.at/feed.xml" nil nil nil)
     ("null program - Chris Wellons"
      "https://nullprogram.com/tags/emacs/feed/" nil nil nil)
     ("Nintendo Life" "https://www.nintendolife.com/feeds/latest" nil
      nil nil)
     ("NixOS Weekly" "https://weekly.nixos.org/feeds/all.rss.xml" nil
      nil nil)
     ("FSF GNU" "http://www.gnu.org/rss/whatsnew.rss" nil nil nil)
     ("FSF News" "https://www.fsf.org/static/fsforg/rss/news.xml" nil
      nil nil)
     ("FSF Blog" "https://www.fsf.org/static/fsforg/rss/blogs.xml" nil
      nil nil)))
 '(newsticker-url-list-defaults
   '(("LWN (Linux Weekly News)" "https://lwn.net/headlines/rss")
     ("Quote of the day"
      "http://feeds.feedburner.com/quotationspage/qotd" "07:00" 86400)
     ("Wired News" "https://www.wired.com/feed/rss")))
 '(org-agenda-files
   '("~/org/inbox.org" "~/org/projects/havila.org"
     "~/org/projects/coolray.org" "~/org/projects/summer-hills.org"
     "~/org/projects/routines.org" "~/org/projects/reading.org"
     "~/org/ideate/20231024135925-ideate_releases.org"))
 '(osm-home '(12.49 122.48 6))
 '(package-selected-packages
   '(avy highlight-indent-guides cape corfu saveplace-pdf-view pcsv gptel
         deadgrep cov pytest prodigy uuidgen blacken org-roam emms
         compat emacsql f git-commit log4e lsp-docker magit-section
         markdown-mode shell-maker transient treemacs
         visual-fill-column with-editor yaml focus elpy chess devdocs
         chatgpt-shell diff-hl org-alert edit-indirect consult-dir
         dired-recent selected calibre osm easy-kill org-present
         org-modern orderless alert chronos multi-term esh-help
         eshell-up eshell-z exec-path-from-shell slime ox-jira ox-gfm
         ox-slack git-timemachine consult marginalia flycheck-package
         csv-mode atomic-chrome solidity-mode dockerfile-mode
         yaml-imenu yaml-mode dash ag js2-refactor xref-js2
         typescript-mode company-go lsp-treemacs lsp-mode go-mode
         ztree nav-flash company-nixos-options nix-mode nixos-options
         dap-mode bui nov highlight-numbers lorem-ipsum
         command-log-mode imenu-list yasnippet package-lint
         currency-convert company-native-complete native-complete
         graphviz-dot-mode company-restclient restclient
         know-your-http-well multiple-cursors inf-clojure clojure-mode
         aggressive-indent tree-mode json-navigator prism
         find-file-in-repository dired-toggle diminish transpose-frame
         amx unfill readline-complete writeroom-mode magit htmlize
         flycheck flycheck-kotlin typing company edit-server js2-mode
         kotlin-mode smartparens json-mode origami browse-kill-ring
         which-key plantuml-mode centered-cursor-mode expand-region
         pdf-tools))
 '(package-vc-selected-packages
   '((saveplace-pdf-view :vc-backend Git :url
                         "https://github.com/nicolaisingh/saveplace-pdf-view.git")))
 '(plantuml-default-exec-mode 'jar)
 '(save-abbrevs 'silently)
 '(tab-bar-auto-width-max '(200 20))
 '(tab-bar-close-button-show nil)
 '(tab-bar-format
   '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator))
 '(tab-bar-show 1)
 '(window-divider-default-right-width 3)
 '(window-divider-mode t)
 '(writeroom-extra-line-spacing 0.3)
 '(writeroom-global-effects
   '(writeroom-set-alpha writeroom-set-menu-bar-lines
                         writeroom-set-tool-bar-lines
                         writeroom-set-vertical-scroll-bars
                         writeroom-set-bottom-divider-width))
 '(writeroom-maximize-window nil)
 '(x-stretch-cursor t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "tomato"))))
 '(emms-metaplaylist-mode-current-face ((t (:inherit emms-metaplaylist-mode-face :inverse-video t :weight bold))))
 '(emms-metaplaylist-mode-face ((t (:inherit default :weight normal))))
 '(emms-playlist-selected-face ((t (:inherit emms-playlist-track-face :inverse-video t :weight bold))))
 '(emms-playlist-track-face ((t (:inherit default))))
 '(hl-line ((t (:background "gray95")))))
