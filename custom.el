(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))
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
 '(osm-home '(12.49 122.48 6))
 '(plantuml-default-exec-mode 'jar)
 '(save-abbrevs 'silently)
 '(tab-bar-auto-width-max '(200 20))
 '(tab-bar-close-button-show nil)
 '(tab-bar-format
   '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator
                            tab-bar-format-align-right
                            tab-bar-format-global))
 '(tab-bar-show 1)
 '(window-divider-default-right-width 3)
 '(window-divider-mode t)
 '(x-stretch-cursor t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
