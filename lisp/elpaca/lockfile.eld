((acp :source "elpaca-menu-lock-file" :recipe
	  (:package "acp" :fetcher github :repo "xenodium/acp.el" :files
				("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
				 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
				 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
				 "docs/*.texinfo"
				 (:exclude ".dir-locals.el" "test.el" "tests.el"
						   "*-test.el" "*-tests.el" "LICENSE"
						   "README*" "*-pkg.el"))
				:source "elpaca-menu-lock-file" :id acp :branch "main"
				:type git :protocol https :inherit t :depth treeless
				:ref "5140f4121156707245567b8fc3072d4c3b5c867f"))
 (adoc-mode :source "elpaca-menu-lock-file" :recipe
			(:package "adoc-mode" :fetcher github :repo
					  "bbatsov/adoc-mode" :files
					  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
					   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					   "doc/*.texinfo" "lisp/*.el" "docs/dir"
					   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					   (:exclude ".dir-locals.el" "test.el" "tests.el"
								 "*-test.el" "*-tests.el" "LICENSE"
								 "README*" "*-pkg.el"))
					  :source "elpaca-menu-lock-file" :id adoc-mode
					  :branch "master" :type git :protocol https
					  :inherit t :depth treeless :ref
					  "5c1484b8982845845ccd0be02629e21f1d5bed81"))
 (agent-shell :source "elpaca-menu-lock-file" :recipe
			  (:package "agent-shell" :fetcher github :repo
						"xenodium/agent-shell" :files
						("*.el" "*.el.in" "dir" "*.info" "*.texi"
						 "*.texinfo" "doc/dir" "doc/*.info"
						 "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
						 "docs/dir" "docs/*.info" "docs/*.texi"
						 "docs/*.texinfo"
						 (:exclude ".dir-locals.el" "test.el"
								   "tests.el" "*-test.el" "*-tests.el"
								   "LICENSE" "README*" "*-pkg.el"))
						:source "elpaca-menu-lock-file" :id
						agent-shell :branch "main" :type git :protocol
						https :inherit t :depth treeless :ref
						"3c8bd0b995c398625b8088a37f46939c0976e3d7"))
 (agent-shell-sidebar :source "elpaca-menu-lock-file" :recipe
					  (:source "elpaca-menu-lock-file" :package
							   "agent-shell-sidebar" :id
							   agent-shell-sidebar :host github :repo
							   "cmacrae/agent-shell-sidebar" :branch
							   "main" :type git :protocol https
							   :inherit t :depth treeless :ref
							   "10fee0b1463cdf210b0908c23e940a44c8d4a8e2"))
 (aggressive-indent :source "elpaca-menu-lock-file" :recipe
					(:package "aggressive-indent" :repo
							  "Malabarba/aggressive-indent-mode"
							  :fetcher github :files
							  ("*.el" "*.el.in" "dir" "*.info"
							   "*.texi" "*.texinfo" "doc/dir"
							   "doc/*.info" "doc/*.texi"
							   "doc/*.texinfo" "lisp/*.el" "docs/dir"
							   "docs/*.info" "docs/*.texi"
							   "docs/*.texinfo"
							   (:exclude ".dir-locals.el" "test.el"
										 "tests.el" "*-test.el"
										 "*-tests.el" "LICENSE"
										 "README*" "*-pkg.el"))
							  :source "elpaca-menu-lock-file" :id
							  aggressive-indent :branch "master" :type
							  git :protocol https :inherit t :depth
							  treeless :ref
							  "a437a45868f94b77362c6b913c5ee8e67b273c42"))
 (alert :source "elpaca-menu-lock-file" :recipe
		(:package "alert" :fetcher github :repo "jwiegley/alert"
				  :files
				  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
				   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
				   "doc/*.texinfo" "lisp/*.el" "docs/dir"
				   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
				   (:exclude ".dir-locals.el" "test.el" "tests.el"
							 "*-test.el" "*-tests.el" "LICENSE"
							 "README*" "*-pkg.el"))
				  :source "elpaca-menu-lock-file" :id alert :branch
				  "master" :type git :protocol https :inherit t :depth
				  treeless :ref
				  "31fc56855289d0846e73d7ca9b84b628aeac16a0"))
 (async :source "elpaca-menu-lock-file" :recipe
		(:package "async" :repo "jwiegley/emacs-async" :fetcher github
				  :files
				  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
				   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
				   "doc/*.texinfo" "lisp/*.el" "docs/dir"
				   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
				   (:exclude ".dir-locals.el" "test.el" "tests.el"
							 "*-test.el" "*-tests.el" "LICENSE"
							 "README*" "*-pkg.el"))
				  :source "elpaca-menu-lock-file" :id async :type git
				  :protocol https :inherit t :depth treeless :ref
				  "5faab28916603bb324d9faba057021ce028ca847"))
 (avy :source "elpaca-menu-lock-file" :recipe
	  (:package "avy" :repo "abo-abo/avy" :fetcher github :files
				("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
				 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
				 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
				 "docs/*.texinfo"
				 (:exclude ".dir-locals.el" "test.el" "tests.el"
						   "*-test.el" "*-tests.el" "LICENSE"
						   "README*" "*-pkg.el"))
				:source "elpaca-menu-lock-file" :id avy :branch
				"master" :type git :protocol https :inherit t :depth
				treeless :ref
				"933d1f36cca0f71e4acb5fac707e9ae26c536264"))
 (blacken :source "elpaca-menu-lock-file" :recipe
		  (:package "blacken" :fetcher github :repo
					"pythonic-emacs/blacken" :files
					("*.el" "*.el.in" "dir" "*.info" "*.texi"
					 "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					 "doc/*.texinfo" "lisp/*.el" "docs/dir"
					 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					 (:exclude ".dir-locals.el" "test.el" "tests.el"
							   "*-test.el" "*-tests.el" "LICENSE"
							   "README*" "*-pkg.el"))
					:source "elpaca-menu-lock-file" :id blacken
					:branch "master" :type git :protocol https
					:inherit t :depth treeless :ref
					"52c412e8b14d6a41e80b8bf8fbbc72a5c21b87e7"))
 (breadcrumb :source "elpaca-menu-lock-file" :recipe
			 (:package "breadcrumb" :repo
					   ("https://github.com/joaotavora/breadcrumb"
						. "breadcrumb")
					   :tar "1.0.1" :host gnu :files
					   ("*" (:exclude ".git")) :source
					   "elpaca-menu-lock-file" :id breadcrumb :branch
					   "master" :type git :protocol https :inherit t
					   :depth treeless :ref
					   "899ddc05c54ba9e005974deeea7518c798c1bd87"))
 (browse-kill-ring :source "elpaca-menu-lock-file" :recipe
				   (:package "browse-kill-ring" :repo
							 "browse-kill-ring/browse-kill-ring"
							 :fetcher github :files
							 ("*.el" "*.el.in" "dir" "*.info" "*.texi"
							  "*.texinfo" "doc/dir" "doc/*.info"
							  "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
							  "docs/dir" "docs/*.info" "docs/*.texi"
							  "docs/*.texinfo"
							  (:exclude ".dir-locals.el" "test.el"
										"tests.el" "*-test.el"
										"*-tests.el" "LICENSE"
										"README*" "*-pkg.el"))
							 :source "elpaca-menu-lock-file" :id
							 browse-kill-ring :branch "master" :type
							 git :protocol https :inherit t :depth
							 treeless :ref
							 "39d65a830b93530c9bd68a7dc14353cbffd1d01f"))
 (calibre :source "elpaca-menu-lock-file" :recipe
		  (:package "calibre" :repo
					("https://git.disroot.org/kjartanoli/calibre.el"
					 . "calibre")
					:tar "1.5.2" :host gnu :files
					("*" (:exclude ".git")) :source
					"elpaca-menu-lock-file" :id calibre :branch
					"master" :type git :protocol https :inherit t
					:depth treeless :ref
					"5f13266520c6135510bf214f67e084e39a97e3b3"))
 (cape :source "elpaca-menu-lock-file" :recipe
	   (:package "cape" :repo "minad/cape" :fetcher github :files
				 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
				  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
				  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
				  "docs/*.texinfo"
				  (:exclude ".dir-locals.el" "test.el" "tests.el"
							"*-test.el" "*-tests.el" "LICENSE"
							"README*" "*-pkg.el"))
				 :source "elpaca-menu-lock-file" :id cape :branch
				 "main" :type git :protocol https :inherit t :depth
				 treeless :ref
				 "73e0023e7fd9f224babfe8795eb9334dba8eec47"))
 (centered-cursor-mode :source "elpaca-menu-lock-file" :recipe
					   (:package "centered-cursor-mode" :fetcher
								 github :repo
								 "andre-r/centered-cursor-mode.el"
								 :files
								 ("*.el" "*.el.in" "dir" "*.info"
								  "*.texi" "*.texinfo" "doc/dir"
								  "doc/*.info" "doc/*.texi"
								  "doc/*.texinfo" "lisp/*.el"
								  "docs/dir" "docs/*.info"
								  "docs/*.texi" "docs/*.texinfo"
								  (:exclude ".dir-locals.el" "test.el"
											"tests.el" "*-test.el"
											"*-tests.el" "LICENSE"
											"README*" "*-pkg.el"))
								 :source "elpaca-menu-lock-file" :id
								 centered-cursor-mode :branch "master"
								 :type git :protocol https :inherit t
								 :depth treeless :ref
								 "67ef719e685407dbc455c7430765e4e685fd95a9"))
 (chess :source "elpaca-menu-lock-file" :recipe
		(:package "chess" :repo
				  ("https://github.com/emacsmirror/gnu_elpa" . "chess")
				  :tar "2.0.5" :host gnu :branch "externals/chess"
				  :files ("*" (:exclude ".git")) :source
				  "elpaca-menu-lock-file" :id chess :type git
				  :protocol https :inherit t :depth treeless :ref
				  "e4adaf1a8e276ff556763aca8a47e10193f2bed4"))
 (chronos :source "elpaca-menu-lock-file" :recipe
		  (:package "chronos" :fetcher github :repo
					"DarkBuffalo/chronos" :files
					("*.el" "*.el.in" "dir" "*.info" "*.texi"
					 "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					 "doc/*.texinfo" "lisp/*.el" "docs/dir"
					 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					 (:exclude ".dir-locals.el" "test.el" "tests.el"
							   "*-test.el" "*-tests.el" "LICENSE"
							   "README*" "*-pkg.el"))
					:source "elpaca-menu-lock-file" :id chronos
					:branch "master" :type git :protocol https
					:inherit t :depth treeless :ref
					"5ea0bf7c3881ea905e280446342539b242401979"))
 (claude-code-ide :source "elpaca-menu-lock-file" :recipe
				  (:source "elpaca-menu-lock-file" :package
						   "claude-code-ide" :id claude-code-ide :host
						   github :repo "manzaltu/claude-code-ide.el"
						   :branch "main" :type git :protocol https
						   :inherit t :depth treeless :ref
						   "cc508396a09e98931bb588da8542b73fa07733e2"))
 (clojure-mode :source "elpaca-menu-lock-file" :recipe
			   (:package "clojure-mode" :repo
						 "clojure-emacs/clojure-mode" :fetcher github
						 :files ("clojure-mode.el") :source
						 "elpaca-menu-lock-file" :id clojure-mode
						 :branch "master" :type git :protocol https
						 :inherit t :depth treeless :ref
						 "3c68569738f04a22d52f1ca28f593c2ee733bf04"))
 (closql :source "elpaca-menu-lock-file" :recipe
		 (:package "closql" :fetcher github :repo "magit/closql"
				   :files
				   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
					"*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					"doc/*.texinfo" "lisp/*.el" "docs/dir"
					"docs/*.info" "docs/*.texi" "docs/*.texinfo"
					(:exclude ".dir-locals.el" "test.el" "tests.el"
							  "*-test.el" "*-tests.el" "LICENSE"
							  "README*" "*-pkg.el"))
				   :source "elpaca-menu-lock-file" :id closql :type
				   git :protocol https :inherit t :depth treeless :ref
				   "d382e7427f5d375ffc872851b049e9f9c4a43dfc"))
 (command-log-mode :source "elpaca-menu-lock-file" :recipe
				   (:package "command-log-mode" :fetcher github :repo
							 "lewang/command-log-mode" :files
							 ("*.el" "*.el.in" "dir" "*.info" "*.texi"
							  "*.texinfo" "doc/dir" "doc/*.info"
							  "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
							  "docs/dir" "docs/*.info" "docs/*.texi"
							  "docs/*.texinfo"
							  (:exclude ".dir-locals.el" "test.el"
										"tests.el" "*-test.el"
										"*-tests.el" "LICENSE"
										"README*" "*-pkg.el"))
							 :source "elpaca-menu-lock-file" :id
							 command-log-mode :branch "master" :type
							 git :protocol https :inherit t :depth
							 treeless :ref
							 "af600e6b4129c8115f464af576505ea8e789db27"))
 (comment-dwim-2 :source "elpaca-menu-lock-file" :recipe
				 (:package "comment-dwim-2" :fetcher github :repo
						   "remyferre/comment-dwim-2" :files
						   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
							"*.texinfo" "doc/dir" "doc/*.info"
							"doc/*.texi" "doc/*.texinfo" "lisp/*.el"
							"docs/dir" "docs/*.info" "docs/*.texi"
							"docs/*.texinfo"
							(:exclude ".dir-locals.el" "test.el"
									  "tests.el" "*-test.el"
									  "*-tests.el" "LICENSE" "README*"
									  "*-pkg.el"))
						   :source "elpaca-menu-lock-file" :id
						   comment-dwim-2 :branch "master" :type git
						   :protocol https :inherit t :depth treeless
						   :ref
						   "6ab75d0a690f0080e9b97c730aac817d04144cd0"))
 (compat :source "elpaca-menu-lock-file" :recipe
		 (:package "compat" :repo
				   ("https://github.com/emacs-compat/compat"
					. "compat")
				   :tar "30.1.0.1" :host gnu :files
				   ("*" (:exclude ".git")) :source
				   "elpaca-menu-lock-file" :id compat :type git
				   :protocol https :inherit t :depth treeless :ref
				   "65c1343f50a29a8c0beb54204abd2b9f0db3c442" :wait t))
 (cond-let
   :source "elpaca-menu-lock-file" :recipe
   (:package "cond-let" :fetcher github :repo "tarsius/cond-let"
			 :files
			 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			  "docs/*.texinfo"
			  (:exclude ".dir-locals.el" "test.el" "tests.el"
						"*-test.el" "*-tests.el" "LICENSE" "README*"
						"*-pkg.el"))
			 :source "elpaca-menu-lock-file" :id cond-let :type git
			 :protocol https :inherit t :depth treeless :ref
			 "c48600dfab6372670225f046cace263700c78eab"))
 (consult :source "elpaca-menu-lock-file" :recipe
		  (:package "consult" :repo "minad/consult" :fetcher github
					:files
					("*.el" "*.el.in" "dir" "*.info" "*.texi"
					 "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					 "doc/*.texinfo" "lisp/*.el" "docs/dir"
					 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					 (:exclude ".dir-locals.el" "test.el" "tests.el"
							   "*-test.el" "*-tests.el" "LICENSE"
							   "README*" "*-pkg.el"))
					:source "elpaca-menu-lock-file" :id consult
					:branch "main" :type git :protocol https :inherit
					t :depth treeless :ref
					"e43b8315bd1a8d9125fff84ad84c4c003e177acb"))
 (consult-dir :source "elpaca-menu-lock-file" :recipe
			  (:package "consult-dir" :fetcher github :repo
						"karthink/consult-dir" :files
						("*.el" "*.el.in" "dir" "*.info" "*.texi"
						 "*.texinfo" "doc/dir" "doc/*.info"
						 "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
						 "docs/dir" "docs/*.info" "docs/*.texi"
						 "docs/*.texinfo"
						 (:exclude ".dir-locals.el" "test.el"
								   "tests.el" "*-test.el" "*-tests.el"
								   "LICENSE" "README*" "*-pkg.el"))
						:source "elpaca-menu-lock-file" :id
						consult-dir :branch "master" :type git
						:protocol https :inherit t :depth treeless
						:ref
						"1497b46d6f48da2d884296a1297e5ace1e050eb5"))
 (corfu :source "elpaca-menu-lock-file" :recipe
		(:package "corfu" :repo "minad/corfu" :files
				  (:defaults "extensions/corfu-*.el") :fetcher github
				  :source "elpaca-menu-lock-file" :id corfu :branch
				  "main" :type git :protocol https :inherit t :depth
				  treeless :ref
				  "c588350d8f9fc60ed0fcf4b78c37671835be01a9"))
 (cov :source "elpaca-menu-lock-file" :recipe
	  (:package "cov" :fetcher github :repo "AdamNiederer/cov" :files
				("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
				 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
				 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
				 "docs/*.texinfo"
				 (:exclude ".dir-locals.el" "test.el" "tests.el"
						   "*-test.el" "*-tests.el" "LICENSE"
						   "README*" "*-pkg.el"))
				:source "elpaca-menu-lock-file" :id cov :branch
				"master" :type git :protocol https :inherit t :depth
				treeless :ref
				"7a3599e42d4fe943b912701e04beffcf2ec812d2"))
 (csv-mode :source "elpaca-menu-lock-file" :recipe
		   (:package "csv-mode" :repo
					 ("https://github.com/emacsmirror/gnu_elpa"
					  . "csv-mode")
					 :tar "1.27" :host gnu :branch
					 "externals/csv-mode" :files
					 ("*" (:exclude ".git")) :source
					 "elpaca-menu-lock-file" :id csv-mode :type git
					 :protocol https :inherit t :depth treeless :ref
					 "3a441a41a3b1d61c2a9c9f5f091cb38ba6b45d47"))
 (currency-convert :source "elpaca-menu-lock-file" :recipe
				   (:package "currency-convert" :fetcher github :repo
							 "lassik/emacs-currency-convert" :files
							 ("*.el" "*.el.in" "dir" "*.info" "*.texi"
							  "*.texinfo" "doc/dir" "doc/*.info"
							  "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
							  "docs/dir" "docs/*.info" "docs/*.texi"
							  "docs/*.texinfo"
							  (:exclude ".dir-locals.el" "test.el"
										"tests.el" "*-test.el"
										"*-tests.el" "LICENSE"
										"README*" "*-pkg.el"))
							 :source "elpaca-menu-lock-file" :id
							 currency-convert :branch "master" :type
							 git :protocol https :inherit t :depth
							 treeless :ref
							 "125a718e73f826f461856aabd19bb2de9327531b"))
 (dash :source "elpaca-menu-lock-file" :recipe
	   (:package "dash" :fetcher github :repo "magnars/dash.el" :files
				 ("dash.el" "dash.texi") :source
				 "elpaca-menu-lock-file" :id dash :type git :protocol
				 https :inherit t :depth treeless :ref
				 "d3a84021dbe48dba63b52ef7665651e0cf02e915"))
 (deadgrep :source "elpaca-menu-lock-file" :recipe
		   (:package "deadgrep" :repo "Wilfred/deadgrep" :fetcher
					 github :files
					 ("*.el" "*.el.in" "dir" "*.info" "*.texi"
					  "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					  "doc/*.texinfo" "lisp/*.el" "docs/dir"
					  "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					  (:exclude ".dir-locals.el" "test.el" "tests.el"
								"*-test.el" "*-tests.el" "LICENSE"
								"README*" "*-pkg.el"))
					 :source "elpaca-menu-lock-file" :id deadgrep
					 :branch "master" :type git :protocol https
					 :inherit t :depth treeless :ref
					 "85257234fa696f412c5701b94f3aa5266a760252"))
 (deflate :source "elpaca-menu-lock-file" :recipe
		  (:package "deflate" :fetcher github :repo "skuro/deflate"
					:files
					("*.el" "*.el.in" "dir" "*.info" "*.texi"
					 "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					 "doc/*.texinfo" "lisp/*.el" "docs/dir"
					 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					 (:exclude ".dir-locals.el" "test.el" "tests.el"
							   "*-test.el" "*-tests.el" "LICENSE"
							   "README*" "*-pkg.el"))
					:source "elpaca-menu-lock-file" :id deflate :type
					git :protocol https :inherit t :depth treeless
					:ref "d3863855d213f73dc7a1a54736d94a75f8f7e9c5"))
 (devdocs :source "elpaca-menu-lock-file" :recipe
		  (:package "devdocs" :fetcher github :repo
					"astoff/devdocs.el" :files
					("*.el" "*.el.in" "dir" "*.info" "*.texi"
					 "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					 "doc/*.texinfo" "lisp/*.el" "docs/dir"
					 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					 (:exclude ".dir-locals.el" "test.el" "tests.el"
							   "*-test.el" "*-tests.el" "LICENSE"
							   "README*" "*-pkg.el"))
					:source "elpaca-menu-lock-file" :id devdocs
					:branch "main" :type git :protocol https :inherit
					t :depth treeless :ref
					"25c746024ddf73570195bf42b841f761a2fee10c"))
 (diff-hl :source "elpaca-menu-lock-file" :recipe
		  (:package "diff-hl" :fetcher github :repo "dgutov/diff-hl"
					:files
					("*.el" "*.el.in" "dir" "*.info" "*.texi"
					 "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					 "doc/*.texinfo" "lisp/*.el" "docs/dir"
					 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					 (:exclude ".dir-locals.el" "test.el" "tests.el"
							   "*-test.el" "*-tests.el" "LICENSE"
							   "README*" "*-pkg.el"))
					:source "elpaca-menu-lock-file" :id diff-hl
					:branch "master" :type git :protocol https
					:inherit t :depth treeless :ref
					"2d7d0714d9637f54af672987c65b6973b31e56a2"))
 (diminish :source "elpaca-menu-lock-file" :recipe
		   (:package "diminish" :fetcher github :repo
					 "myrjola/diminish.el" :files
					 ("*.el" "*.el.in" "dir" "*.info" "*.texi"
					  "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					  "doc/*.texinfo" "lisp/*.el" "docs/dir"
					  "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					  (:exclude ".dir-locals.el" "test.el" "tests.el"
								"*-test.el" "*-tests.el" "LICENSE"
								"README*" "*-pkg.el"))
					 :source "elpaca-menu-lock-file" :id diminish
					 :wait t :branch "master" :type git :protocol
					 https :inherit t :depth treeless :ref
					 "fbd5d846611bad828e336b25d2e131d1bc06b83d"))
 (dired-hacks-utils :source "elpaca-menu-lock-file" :recipe
					(:package "dired-hacks-utils" :fetcher github
							  :repo "Fuco1/dired-hacks" :files
							  ("dired-hacks-utils.el") :source
							  "elpaca-menu-lock-file" :id
							  dired-hacks-utils :type git :protocol
							  https :inherit t :depth treeless :ref
							  "de9336f4b47ef901799fe95315fa080fa6d77b48"))
 (dired-narrow :source "elpaca-menu-lock-file" :recipe
			   (:package "dired-narrow" :fetcher github :repo
						 "Fuco1/dired-hacks" :files
						 ("dired-narrow.el") :source
						 "elpaca-menu-lock-file" :id dired-narrow
						 :branch "master" :type git :protocol https
						 :inherit t :depth treeless :ref
						 "de9336f4b47ef901799fe95315fa080fa6d77b48"))
 (dired-rainbow :source "elpaca-menu-lock-file" :recipe
				(:package "dired-rainbow" :fetcher github :repo
						  "Fuco1/dired-hacks" :files
						  ("dired-rainbow.el") :source
						  "elpaca-menu-lock-file" :id dired-rainbow
						  :branch "master" :type git :protocol https
						  :inherit t :depth treeless :ref
						  "de9336f4b47ef901799fe95315fa080fa6d77b48"))
 (dired-sidebar :source "elpaca-menu-lock-file" :recipe
				(:package "dired-sidebar" :fetcher github :repo
						  "jojojames/dired-sidebar" :files
						  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
						   "*.texinfo" "doc/dir" "doc/*.info"
						   "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
						   "docs/dir" "docs/*.info" "docs/*.texi"
						   "docs/*.texinfo"
						   (:exclude ".dir-locals.el" "test.el"
									 "tests.el" "*-test.el"
									 "*-tests.el" "LICENSE" "README*"
									 "*-pkg.el"))
						  :source "elpaca-menu-lock-file" :id
						  dired-sidebar :branch "master" :type git
						  :protocol https :inherit t :depth treeless
						  :ref
						  "1852a0b17bf2619607f6b4dfc437e279ba04e93c"))
 (dired-subtree :source "elpaca-menu-lock-file" :recipe
				(:package "dired-subtree" :fetcher github :repo
						  "Fuco1/dired-hacks" :files
						  ("dired-subtree.el") :source
						  "elpaca-menu-lock-file" :id dired-subtree
						  :branch "master" :type git :protocol https
						  :inherit t :depth treeless :ref
						  "de9336f4b47ef901799fe95315fa080fa6d77b48"))
 (dockerfile-mode :source "elpaca-menu-lock-file" :recipe
				  (:package "dockerfile-mode" :fetcher github :repo
							"spotify/dockerfile-mode" :files
							("*.el" "*.el.in" "dir" "*.info" "*.texi"
							 "*.texinfo" "doc/dir" "doc/*.info"
							 "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
							 "docs/dir" "docs/*.info" "docs/*.texi"
							 "docs/*.texinfo"
							 (:exclude ".dir-locals.el" "test.el"
									   "tests.el" "*-test.el"
									   "*-tests.el" "LICENSE"
									   "README*" "*-pkg.el"))
							:source "elpaca-menu-lock-file" :id
							dockerfile-mode :branch "master" :type git
							:protocol https :inherit t :depth treeless
							:ref
							"97733ce074b1252c1270fd5e8a53d178b66668ed"))
 (easy-kill :source "elpaca-menu-lock-file" :recipe
			(:package "easy-kill" :fetcher github :repo
					  "leoliu/easy-kill" :files
					  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
					   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					   "doc/*.texinfo" "lisp/*.el" "docs/dir"
					   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					   (:exclude ".dir-locals.el" "test.el" "tests.el"
								 "*-test.el" "*-tests.el" "LICENSE"
								 "README*" "*-pkg.el"))
					  :source "elpaca-menu-lock-file" :id easy-kill
					  :branch "master" :type git :protocol https
					  :inherit t :depth treeless :ref
					  "98cbae5d8c378ad14d612d7c88a78484c49a80b8"))
 (easy-kill-extras :source "elpaca-menu-lock-file" :recipe
				   (:package "easy-kill-extras" :fetcher github :repo
							 "knu/easy-kill-extras.el" :files
							 ("*.el" "*.el.in" "dir" "*.info" "*.texi"
							  "*.texinfo" "doc/dir" "doc/*.info"
							  "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
							  "docs/dir" "docs/*.info" "docs/*.texi"
							  "docs/*.texinfo"
							  (:exclude ".dir-locals.el" "test.el"
										"tests.el" "*-test.el"
										"*-tests.el" "LICENSE"
										"README*" "*-pkg.el"))
							 :source "elpaca-menu-lock-file" :id
							 easy-kill-extras :branch "master" :type
							 git :protocol https :inherit t :depth
							 treeless :ref
							 "3fea06322d76be78e66074b9a9dad9a6d4aa6bcd"))
 (eat :source "elpaca-menu-lock-file" :recipe
	  (:package "eat" :repo
				("https://codeberg.org/akib/emacs-eat" . "eat") :tar
				"0.9.4" :host nongnu :files ("*" (:exclude ".git"))
				:source "elpaca-menu-lock-file" :id eat :branch
				"master" :type git :protocol https :inherit t :depth
				treeless :ref
				"c8d54d649872bfe7b2b9f49ae5c2addbf12d3b99"))
 (eca :source "elpaca-menu-lock-file" :recipe
	  (:package "eca" :fetcher github :repo
				"editor-code-assistant/eca-emacs" :files
				("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
				 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
				 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
				 "docs/*.texinfo"
				 (:exclude ".dir-locals.el" "test.el" "tests.el"
						   "*-test.el" "*-tests.el" "LICENSE"
						   "README*" "*-pkg.el"))
				:source "elpaca-menu-lock-file" :id eca :branch
				"master" :type git :protocol https :inherit t :depth
				treeless :ref
				"f04a58052576789f1807112cb7494478bf45a412"))
 (edit-indirect :source "elpaca-menu-lock-file" :recipe
				(:package "edit-indirect" :fetcher github :repo
						  "Fanael/edit-indirect" :files
						  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
						   "*.texinfo" "doc/dir" "doc/*.info"
						   "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
						   "docs/dir" "docs/*.info" "docs/*.texi"
						   "docs/*.texinfo"
						   (:exclude ".dir-locals.el" "test.el"
									 "tests.el" "*-test.el"
									 "*-tests.el" "LICENSE" "README*"
									 "*-pkg.el"))
						  :source "elpaca-menu-lock-file" :id
						  edit-indirect :branch "master" :type git
						  :protocol https :inherit t :depth treeless
						  :ref
						  "82a28d8a85277cfe453af464603ea330eae41c05"))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source nil :package "elpaca" :id elpaca :repo
			"https://github.com/progfolio/elpaca.git" :ref
			"7f0f635bb59d1e32f17c44c8855aaa2003070214" :depth 1
			:inherit ignore :files
			(:defaults "elpaca-test.el" (:exclude "extensions"))
			:build (:not elpaca-activate) :type git :protocol https))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
					 (:package "elpaca-use-package" :wait t :repo
							   "https://github.com/progfolio/elpaca.git"
							   :files
							   ("extensions/elpaca-use-package.el")
							   :main
							   "extensions/elpaca-use-package.el"
							   :build
							   (:not elpaca-source elpaca-build-docs)
							   :source "Elpaca extensions" :id
							   elpaca-use-package :type git :protocol
							   https :inherit t :depth treeless :ref
							   "7f0f635bb59d1e32f17c44c8855aaa2003070214"))
 (elquery :source "elpaca-menu-lock-file" :recipe
		  (:package "elquery" :fetcher github :repo
					"AdamNiederer/elquery" :files
					("*.el" "*.el.in" "dir" "*.info" "*.texi"
					 "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					 "doc/*.texinfo" "lisp/*.el" "docs/dir"
					 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					 (:exclude ".dir-locals.el" "test.el" "tests.el"
							   "*-test.el" "*-tests.el" "LICENSE"
							   "README*" "*-pkg.el"))
					:source "elpaca-menu-lock-file" :id elquery :type
					git :protocol https :inherit t :depth treeless
					:ref "38f3bd41096cb270919b06095da0b9ac1add4598"))
 (emacsql :source "elpaca-menu-lock-file" :recipe
		  (:package "emacsql" :fetcher github :repo "magit/emacsql"
					:files (:defaults "README.md" "sqlite") :source
					"elpaca-menu-lock-file" :id emacsql :type git
					:protocol https :inherit t :depth treeless :ref
					"d811bbefcb5e27841af55cae53aa939ba720de77"))
 (emms :source "elpaca-menu-lock-file" :recipe
	   (:package "emms" :fetcher git :url
				 "https://git.savannah.gnu.org/git/emms.git" :files
				 (:defaults (:exclude "doc/fdl.texi" "doc/gpl.texi"))
				 :source "GNU ELPA" :id emms :branch "master" :type
				 git :protocol https :inherit t :depth treeless :ref
				 "6f704fd3c12f35e059700aaeb43ae64c35ad81b2"))
 (emojify :source "elpaca-menu-lock-file" :recipe
		  (:package "emojify" :fetcher github :repo
					"iqbalansari/emacs-emojify" :files
					(:defaults "data" "images") :source
					"elpaca-menu-lock-file" :id emojify :branch
					"master" :type git :protocol https :inherit t
					:depth treeless :ref
					"1b726412f19896abf5e4857d4c32220e33400b55"))
 (eshell-toggle :source "elpaca-menu-lock-file" :recipe
				(:package "eshell-toggle" :repo "4DA/eshell-toggle"
						  :fetcher github :files
						  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
						   "*.texinfo" "doc/dir" "doc/*.info"
						   "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
						   "docs/dir" "docs/*.info" "docs/*.texi"
						   "docs/*.texinfo"
						   (:exclude ".dir-locals.el" "test.el"
									 "tests.el" "*-test.el"
									 "*-tests.el" "LICENSE" "README*"
									 "*-pkg.el"))
						  :source "elpaca-menu-lock-file" :id
						  eshell-toggle :branch "master" :type git
						  :protocol https :inherit t :depth treeless
						  :ref
						  "04e501e02c475bd9067eebcf8807c951f2316194"))
 (eshell-up :source "elpaca-menu-lock-file" :recipe
			(:package "eshell-up" :fetcher github :repo
					  "peterwvj/eshell-up" :files
					  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
					   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					   "doc/*.texinfo" "lisp/*.el" "docs/dir"
					   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					   (:exclude ".dir-locals.el" "test.el" "tests.el"
								 "*-test.el" "*-tests.el" "LICENSE"
								 "README*" "*-pkg.el"))
					  :source "elpaca-menu-lock-file" :id eshell-up
					  :branch "master" :type git :protocol https
					  :inherit t :depth treeless :ref
					  "1999afaa509204b780db44e99ac9648fe7d92d32"))
 (eshell-z :source "elpaca-menu-lock-file" :recipe
		   (:package "eshell-z" :fetcher github :repo
					 "xuchunyang/eshell-z" :files
					 ("*.el" "*.el.in" "dir" "*.info" "*.texi"
					  "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					  "doc/*.texinfo" "lisp/*.el" "docs/dir"
					  "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					  (:exclude ".dir-locals.el" "test.el" "tests.el"
								"*-test.el" "*-tests.el" "LICENSE"
								"README*" "*-pkg.el"))
					 :source "elpaca-menu-lock-file" :id eshell-z
					 :branch "master" :type git :protocol https
					 :inherit t :depth treeless :ref
					 "337cb241e17bd472bd3677ff166a0800f684213c"))
 (esxml :source "elpaca-menu-lock-file" :recipe
		(:package "esxml" :fetcher github :repo "tali713/esxml" :files
				  ("esxml.el" "esxml-query.el") :source
				  "elpaca-menu-lock-file" :id esxml :type git
				  :protocol https :inherit t :depth treeless :ref
				  "6a375888a74b7563eb53176aad81faee8b858189"))
 (eterm-256color :source "elpaca-menu-lock-file" :recipe
				 (:package "eterm-256color" :fetcher github :repo
						   "dieggsy/eterm-256color" :files
						   (:defaults "eterm-256color.ti") :source
						   "elpaca-menu-lock-file" :id eterm-256color
						   :branch "main" :type git :protocol https
						   :inherit t :depth treeless :ref
						   "868eeaa958de1deab690fe8ac8f5477452ccdb6a"))
 (expand-region :source "elpaca-menu-lock-file" :recipe
				(:package "expand-region" :repo
						  "magnars/expand-region.el" :fetcher github
						  :files
						  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
						   "*.texinfo" "doc/dir" "doc/*.info"
						   "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
						   "docs/dir" "docs/*.info" "docs/*.texi"
						   "docs/*.texinfo"
						   (:exclude ".dir-locals.el" "test.el"
									 "tests.el" "*-test.el"
									 "*-tests.el" "LICENSE" "README*"
									 "*-pkg.el"))
						  :source "elpaca-menu-lock-file" :id
						  expand-region :branch "master" :type git
						  :protocol https :inherit t :depth treeless
						  :ref
						  "351279272330cae6cecea941b0033a8dd8bcc4e8"))
 (f :source "elpaca-menu-lock-file" :recipe
	(:package "f" :fetcher github :repo "rejeep/f.el" :files
			  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			   "docs/*.texinfo"
			   (:exclude ".dir-locals.el" "test.el" "tests.el"
						 "*-test.el" "*-tests.el" "LICENSE" "README*"
						 "*-pkg.el"))
			  :source "elpaca-menu-lock-file" :id f :type git
			  :protocol https :inherit t :depth treeless :ref
			  "931b6d0667fe03e7bf1c6c282d6d8d7006143c52"))
 (focus :source "elpaca-menu-lock-file" :recipe
		(:package "focus" :fetcher github :repo "larstvei/Focus"
				  :files
				  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
				   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
				   "doc/*.texinfo" "lisp/*.el" "docs/dir"
				   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
				   (:exclude ".dir-locals.el" "test.el" "tests.el"
							 "*-test.el" "*-tests.el" "LICENSE"
							 "README*" "*-pkg.el"))
				  :source "elpaca-menu-lock-file" :id focus :branch
				  "master" :type git :protocol https :inherit t :depth
				  treeless :ref
				  "a58e29e70948512dbcdbb24745e3fb0a59984925"))
 (forge :source "elpaca-menu-lock-file" :recipe
		(:package "forge" :fetcher github :repo "magit/forge" :files
				  ("lisp/*.el" "docs/*.texi" ".dir-locals.el") :source
				  "elpaca-menu-lock-file" :id forge :branch "main"
				  :type git :protocol https :inherit t :depth treeless
				  :ref "9628f76740aec9270e9fb31457ff4cb38d9f3f16"))
 (form-feed :source "elpaca-menu-lock-file" :recipe
			(:package "form-feed" :fetcher git :url
					  "https://depp.brause.cc/form-feed.git" :files
					  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
					   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					   "doc/*.texinfo" "lisp/*.el" "docs/dir"
					   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					   (:exclude ".dir-locals.el" "test.el" "tests.el"
								 "*-test.el" "*-tests.el" "LICENSE"
								 "README*" "*-pkg.el"))
					  :source "elpaca-menu-lock-file" :id form-feed
					  :branch "master" :type git :protocol https
					  :inherit t :depth treeless :ref
					  "6258fe6390a7bf264a6f02813502bf83a645d872"))
 (free-keys :source "elpaca-menu-lock-file" :recipe
			(:package "free-keys" :fetcher github :repo
					  "Fuco1/free-keys" :files
					  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
					   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					   "doc/*.texinfo" "lisp/*.el" "docs/dir"
					   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					   (:exclude ".dir-locals.el" "test.el" "tests.el"
								 "*-test.el" "*-tests.el" "LICENSE"
								 "README*" "*-pkg.el"))
					  :source "elpaca-menu-lock-file" :id free-keys
					  :branch "master" :type git :protocol https
					  :inherit t :depth treeless :ref
					  "bed8e9c356c889cd98dd7a4a63c69d6c4960cf82"))
 (ghub :source "elpaca-menu-lock-file" :recipe
	   (:package "ghub" :fetcher github :repo "magit/ghub" :files
				 ("lisp/*.el" "docs/*.texi" ".dir-locals.el") :source
				 "elpaca-menu-lock-file" :id ghub :type git :protocol
				 https :inherit t :depth treeless :ref
				 "59d0b9b33e780d6cff5131886904ff26033dd2e6"))
 (git-timemachine :source "elpaca-menu-lock-file" :recipe
				  (:package "git-timemachine" :fetcher codeberg :repo
							"pidu/git-timemachine" :files
							("*.el" "*.el.in" "dir" "*.info" "*.texi"
							 "*.texinfo" "doc/dir" "doc/*.info"
							 "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
							 "docs/dir" "docs/*.info" "docs/*.texi"
							 "docs/*.texinfo"
							 (:exclude ".dir-locals.el" "test.el"
									   "tests.el" "*-test.el"
									   "*-tests.el" "LICENSE"
									   "README*" "*-pkg.el"))
							:source "elpaca-menu-lock-file" :id
							git-timemachine :branch "master" :type git
							:protocol https :inherit t :depth treeless
							:ref
							"d1346a76122595aeeb7ebb292765841c6cfd417b"))
 (gntp :source "elpaca-menu-lock-file" :recipe
	   (:package "gntp" :repo "tekai/gntp.el" :fetcher github :files
				 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
				  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
				  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
				  "docs/*.texinfo"
				  (:exclude ".dir-locals.el" "test.el" "tests.el"
							"*-test.el" "*-tests.el" "LICENSE"
							"README*" "*-pkg.el"))
				 :source "elpaca-menu-lock-file" :id gntp :type git
				 :protocol https :inherit t :depth treeless :ref
				 "767571135e2c0985944017dc59b0be79af222ef5"))
 (go-mode :source "elpaca-menu-lock-file" :recipe
		  (:package "go-mode" :repo "dominikh/go-mode.el" :fetcher
					github :files ("go-mode.el") :source
					"elpaca-menu-lock-file" :id go-mode :branch
					"master" :type git :protocol https :inherit t
					:depth treeless :ref
					"3a71d28ab47df685e54ca6046a7a3dd3e28b682c"))
 (goggles :source "elpaca-menu-lock-file" :recipe
		  (:package "goggles" :repo "minad/goggles" :fetcher github
					:files
					("*.el" "*.el.in" "dir" "*.info" "*.texi"
					 "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					 "doc/*.texinfo" "lisp/*.el" "docs/dir"
					 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					 (:exclude ".dir-locals.el" "test.el" "tests.el"
							   "*-test.el" "*-tests.el" "LICENSE"
							   "README*" "*-pkg.el"))
					:source "elpaca-menu-lock-file" :id goggles
					:branch "main" :type git :protocol https :inherit
					t :depth treeless :ref
					"700a61cf18da9e66a3346499749a76db8dce6b31"))
 (gptel :source "elpaca-menu-lock-file" :recipe
		(:package "gptel" :repo "karthink/gptel" :fetcher github
				  :files
				  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
				   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
				   "doc/*.texinfo" "lisp/*.el" "docs/dir"
				   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
				   (:exclude ".dir-locals.el" "test.el" "tests.el"
							 "*-test.el" "*-tests.el" "LICENSE"
							 "README*" "*-pkg.el"))
				  :source "elpaca-menu-lock-file" :id gptel :branch
				  "master" :type git :protocol https :inherit t :depth
				  treeless :ref
				  "74cd61c58b0922c020683677085e3b0deb0c044d"))
 (graphviz-dot-mode :source "elpaca-menu-lock-file" :recipe
					(:package "graphviz-dot-mode" :repo
							  "ppareit/graphviz-dot-mode" :fetcher
							  github :files
							  ("*.el" "*.el.in" "dir" "*.info"
							   "*.texi" "*.texinfo" "doc/dir"
							   "doc/*.info" "doc/*.texi"
							   "doc/*.texinfo" "lisp/*.el" "docs/dir"
							   "docs/*.info" "docs/*.texi"
							   "docs/*.texinfo"
							   (:exclude ".dir-locals.el" "test.el"
										 "tests.el" "*-test.el"
										 "*-tests.el" "LICENSE"
										 "README*" "*-pkg.el"))
							  :source "elpaca-menu-lock-file" :id
							  graphviz-dot-mode :branch "master" :type
							  git :protocol https :inherit t :depth
							  treeless :ref
							  "516c151b845a3eb2da73eb4ee648ad99172087ac"))
 (hcl-mode :source "elpaca-menu-lock-file" :recipe
		   (:package "hcl-mode" :repo "hcl-emacs/hcl-mode" :fetcher
					 github :files
					 ("*.el" "*.el.in" "dir" "*.info" "*.texi"
					  "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					  "doc/*.texinfo" "lisp/*.el" "docs/dir"
					  "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					  (:exclude ".dir-locals.el" "test.el" "tests.el"
								"*-test.el" "*-tests.el" "LICENSE"
								"README*" "*-pkg.el"))
					 :source "elpaca-menu-lock-file" :id hcl-mode
					 :type git :protocol https :inherit t :depth
					 treeless :ref
					 "1da895ed75d28d9f87cbf9b74f075d90ba31c0ed"))
 (highlight-indent-guides :source "elpaca-menu-lock-file" :recipe
						  (:package "highlight-indent-guides" :fetcher
									github :repo
									"bumblepup/highlight-indent-guides"
									:files
									("*.el" "*.el.in" "dir" "*.info"
									 "*.texi" "*.texinfo" "doc/dir"
									 "doc/*.info" "doc/*.texi"
									 "doc/*.texinfo" "lisp/*.el"
									 "docs/dir" "docs/*.info"
									 "docs/*.texi" "docs/*.texinfo"
									 (:exclude ".dir-locals.el"
											   "test.el" "tests.el"
											   "*-test.el"
											   "*-tests.el" "LICENSE"
											   "README*" "*-pkg.el"))
									:source "elpaca-menu-lock-file"
									:id highlight-indent-guides
									:branch "master" :type git
									:protocol https :inherit t :depth
									treeless :ref
									"08a0859d4af2d0ce95be2a1e9aa5d1fd5ac42792"))
 (highlight-numbers :source "elpaca-menu-lock-file" :recipe
					(:package "highlight-numbers" :fetcher github
							  :repo "Fanael/highlight-numbers"
							  :old-names (number-font-lock-mode)
							  :files
							  ("*.el" "*.el.in" "dir" "*.info"
							   "*.texi" "*.texinfo" "doc/dir"
							   "doc/*.info" "doc/*.texi"
							   "doc/*.texinfo" "lisp/*.el" "docs/dir"
							   "docs/*.info" "docs/*.texi"
							   "docs/*.texinfo"
							   (:exclude ".dir-locals.el" "test.el"
										 "tests.el" "*-test.el"
										 "*-tests.el" "LICENSE"
										 "README*" "*-pkg.el"))
							  :source "elpaca-menu-lock-file" :id
							  highlight-numbers :branch "master" :type
							  git :protocol https :inherit t :depth
							  treeless :ref
							  "8b4744c7f46c72b1d3d599d4fb75ef8183dee307"))
 (hl-todo :source "elpaca-menu-lock-file" :recipe
		  (:package "hl-todo" :repo "tarsius/hl-todo" :fetcher github
					:files
					("*.el" "*.el.in" "dir" "*.info" "*.texi"
					 "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					 "doc/*.texinfo" "lisp/*.el" "docs/dir"
					 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					 (:exclude ".dir-locals.el" "test.el" "tests.el"
							   "*-test.el" "*-tests.el" "LICENSE"
							   "README*" "*-pkg.el"))
					:source "elpaca-menu-lock-file" :id hl-todo :type
					git :protocol https :inherit t :depth treeless
					:ref "527d545b8c2f36243194cbe4a8d0e6ac9d50e6a7"))
 (howm :source "elpaca-menu-lock-file" :recipe
	   (:package "howm" :fetcher github :repo "kaorahi/howm" :files
				 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
				  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
				  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
				  "docs/*.texinfo"
				  (:exclude ".dir-locals.el" "test.el" "tests.el"
							"*-test.el" "*-tests.el" "LICENSE"
							"README*" "*-pkg.el"))
				 :source "elpaca-menu-lock-file" :id howm :protocol
				 ssh :remotes (("fork" :repo "nicolaisingh/howm"))
				 :branch "master" :type git :inherit t :depth treeless
				 :ref "d1431c9e678de6e0c179b2a2bf405e8804472fee"))
 (ht :source "elpaca-menu-lock-file" :recipe
	 (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
			   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
				"doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
				"lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
				"docs/*.texinfo"
				(:exclude ".dir-locals.el" "test.el" "tests.el"
						  "*-test.el" "*-tests.el" "LICENSE" "README*"
						  "*-pkg.el"))
			   :source "elpaca-menu-lock-file" :id ht :type git
			   :protocol https :inherit t :depth treeless :ref
			   "1c49aad1c820c86f7ee35bf9fff8429502f60fef"))
 (htmlize :source "elpaca-menu-lock-file" :recipe
		  (:package "htmlize" :fetcher github :repo
					"emacsorphanage/htmlize" :files
					("*.el" "*.el.in" "dir" "*.info" "*.texi"
					 "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					 "doc/*.texinfo" "lisp/*.el" "docs/dir"
					 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					 (:exclude ".dir-locals.el" "test.el" "tests.el"
							   "*-test.el" "*-tests.el" "LICENSE"
							   "README*" "*-pkg.el"))
					:source "elpaca-menu-lock-file" :id htmlize
					:branch "master" :type git :protocol https
					:inherit t :depth treeless :ref
					"fa644880699adea3770504f913e6dddbec90c076"))
 (hydra :source "elpaca-menu-lock-file" :recipe
		(:package "hydra" :repo "abo-abo/hydra" :fetcher github :files
				  (:defaults (:exclude "lv.el")) :source
				  "elpaca-menu-lock-file" :id hydra :type git
				  :protocol https :inherit t :depth treeless :ref
				  "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (json-navigator :source "elpaca-menu-lock-file" :recipe
				 (:package "json-navigator" :fetcher github :repo
						   "DamienCassou/json-navigator" :files
						   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
							"*.texinfo" "doc/dir" "doc/*.info"
							"doc/*.texi" "doc/*.texinfo" "lisp/*.el"
							"docs/dir" "docs/*.info" "docs/*.texi"
							"docs/*.texinfo"
							(:exclude ".dir-locals.el" "test.el"
									  "tests.el" "*-test.el"
									  "*-tests.el" "LICENSE" "README*"
									  "*-pkg.el"))
						   :source "elpaca-menu-lock-file" :id
						   json-navigator :branch "master" :type git
						   :protocol https :inherit t :depth treeless
						   :ref
						   "8ab49b066bc23de731a29ef07bbafa29999e1852"))
 (know-your-http-well :source "elpaca-menu-lock-file" :recipe
					  (:package "know-your-http-well" :fetcher github
								:repo "for-GET/know-your-http-well"
								:files ("emacs/*.el") :source
								"elpaca-menu-lock-file" :id
								know-your-http-well :branch "master"
								:type git :protocol https :inherit t
								:depth treeless :ref
								"c916e825b0c75c2f2b6ff5ba79c981cff7de5797"))
 (llama :source "elpaca-menu-lock-file" :recipe
		(:package "llama" :fetcher github :repo "tarsius/llama" :files
				  ("llama.el" ".dir-locals.el") :source
				  "elpaca-menu-lock-file" :id llama :type git
				  :protocol https :inherit t :depth treeless :ref
				  "4d4024048053b898a01521046e0f063ee47615b0"))
 (llm-tool-collection :source "elpaca-menu-lock-file" :recipe
					  (:source "elpaca-menu-lock-file" :package
							   "llm-tool-collection" :id
							   llm-tool-collection :repo
							   "~/.emacs.d/packages/llm-tool-collection"
							   :branch "main" :type git :protocol
							   https :inherit t :depth treeless :ref
							   "b9fd45bedf3e0fb07d289730991199ae18785157"))
 (log4e :source "elpaca-menu-lock-file" :recipe
		(:package "log4e" :repo "aki2o/log4e" :fetcher github :files
				  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
				   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
				   "doc/*.texinfo" "lisp/*.el" "docs/dir"
				   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
				   (:exclude ".dir-locals.el" "test.el" "tests.el"
							 "*-test.el" "*-tests.el" "LICENSE"
							 "README*" "*-pkg.el"))
				  :source "elpaca-menu-lock-file" :id log4e :type git
				  :protocol https :inherit t :depth treeless :ref
				  "6d71462df9bf595d3861bfb328377346aceed422"))
 (lorem-ipsum :source "elpaca-menu-lock-file" :recipe
			  (:package "lorem-ipsum" :fetcher github :repo
						"jschaf/emacs-lorem-ipsum" :files
						("*.el" "*.el.in" "dir" "*.info" "*.texi"
						 "*.texinfo" "doc/dir" "doc/*.info"
						 "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
						 "docs/dir" "docs/*.info" "docs/*.texi"
						 "docs/*.texinfo"
						 (:exclude ".dir-locals.el" "test.el"
								   "tests.el" "*-test.el" "*-tests.el"
								   "LICENSE" "README*" "*-pkg.el"))
						:source "elpaca-menu-lock-file" :id
						lorem-ipsum :branch "master" :type git
						:protocol https :inherit t :depth treeless
						:ref
						"4e87a899868e908a7a9e1812831d76c8d072f885"))
 (lv :source "elpaca-menu-lock-file" :recipe
	 (:package "lv" :repo "abo-abo/hydra" :fetcher github :files
			   ("lv.el") :source "elpaca-menu-lock-file" :id lv :type
			   git :protocol https :inherit t :depth treeless :ref
			   "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (macrostep :source "elpaca-menu-lock-file" :recipe
			(:package "macrostep" :fetcher github :repo
					  "emacsorphanage/macrostep" :files
					  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
					   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					   "doc/*.texinfo" "lisp/*.el" "docs/dir"
					   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					   (:exclude ".dir-locals.el" "test.el" "tests.el"
								 "*-test.el" "*-tests.el" "LICENSE"
								 "README*" "*-pkg.el"))
					  :source "elpaca-menu-lock-file" :id macrostep
					  :type git :protocol https :inherit t :depth
					  treeless :ref
					  "d0928626b4711dcf9f8f90439d23701118724199"))
 (magit :source "elpaca-menu-lock-file" :recipe
		(:package "magit" :fetcher github :repo "magit/magit" :files
				  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi"
				   "docs/AUTHORS.md" "LICENSE" ".dir-locals.el"
				   ("git-hooks" "git-hooks/*")
				   (:exclude "lisp/magit-section.el"))
				  :source "elpaca-menu-lock-file" :id magit :branch
				  "main" :type git :protocol https :inherit t :depth
				  treeless :ref
				  "6433b8ceebc59bbf6824553a76a07f54cab192ec"))
 (magit-section :source "elpaca-menu-lock-file" :recipe
				(:package "magit-section" :fetcher github :repo
						  "magit/magit" :files
						  ("lisp/magit-section.el"
						   "docs/magit-section.texi"
						   "magit-section-pkg.el")
						  :source "elpaca-menu-lock-file" :id
						  magit-section :type git :protocol https
						  :inherit t :depth treeless :ref
						  "6433b8ceebc59bbf6824553a76a07f54cab192ec"))
 (magit-todos :source "elpaca-menu-lock-file" :recipe
			  (:package "magit-todos" :fetcher github :repo
						"alphapapa/magit-todos" :files
						("*.el" "*.el.in" "dir" "*.info" "*.texi"
						 "*.texinfo" "doc/dir" "doc/*.info"
						 "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
						 "docs/dir" "docs/*.info" "docs/*.texi"
						 "docs/*.texinfo"
						 (:exclude ".dir-locals.el" "test.el"
								   "tests.el" "*-test.el" "*-tests.el"
								   "LICENSE" "README*" "*-pkg.el"))
						:source "elpaca-menu-lock-file" :id
						magit-todos :branch "master" :type git
						:protocol https :inherit t :depth treeless
						:ref
						"7294a95580bddf7232f2d205efae312dc24c5f61"))
 (marginalia :source "elpaca-menu-lock-file" :recipe
			 (:package "marginalia" :repo "minad/marginalia" :fetcher
					   github :files
					   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
						"*.texinfo" "doc/dir" "doc/*.info"
						"doc/*.texi" "doc/*.texinfo" "lisp/*.el"
						"docs/dir" "docs/*.info" "docs/*.texi"
						"docs/*.texinfo"
						(:exclude ".dir-locals.el" "test.el"
								  "tests.el" "*-test.el" "*-tests.el"
								  "LICENSE" "README*" "*-pkg.el"))
					   :source "elpaca-menu-lock-file" :id marginalia
					   :branch "main" :type git :protocol https
					   :inherit t :depth treeless :ref
					   "7604eb0463994b0528b92aaf6e594f64089b8b95"))
 (markdown-mode :source "elpaca-menu-lock-file" :recipe
				(:package "markdown-mode" :fetcher github :repo
						  "jrblevin/markdown-mode" :files
						  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
						   "*.texinfo" "doc/dir" "doc/*.info"
						   "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
						   "docs/dir" "docs/*.info" "docs/*.texi"
						   "docs/*.texinfo"
						   (:exclude ".dir-locals.el" "test.el"
									 "tests.el" "*-test.el"
									 "*-tests.el" "LICENSE" "README*"
									 "*-pkg.el"))
						  :source "elpaca-menu-lock-file" :id
						  markdown-mode :branch "master" :type git
						  :protocol https :inherit t :depth treeless
						  :ref
						  "1f72cefa6a4b759f90e335e4908725a721b17ad9"))
 (mcp :source "elpaca-menu-lock-file" :recipe
	  (:package "mcp" :fetcher github :repo "lizqwerscott/mcp.el"
				:files
				("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
				 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
				 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
				 "docs/*.texinfo"
				 (:exclude ".dir-locals.el" "test.el" "tests.el"
						   "*-test.el" "*-tests.el" "LICENSE"
						   "README*" "*-pkg.el"))
				:source "elpaca-menu-lock-file" :id mcp :branch
				"master" :type git :protocol https :inherit t :depth
				treeless :ref
				"2d172809cbdb2a40d86b28ad73bd65547cefe0e1"))
 (memory-usage :source "elpaca-menu-lock-file" :recipe
			   (:package "memory-usage" :repo
						 ("https://github.com/emacsmirror/gnu_elpa"
						  . "memory-usage")
						 :tar "0.2" :host gnu :branch
						 "externals/memory-usage" :files
						 ("*" (:exclude ".git")) :source
						 "elpaca-menu-lock-file" :id memory-usage
						 :type git :protocol https :inherit t :depth
						 treeless :ref
						 "c751101ba3adaca67a37977562907557963007bb"))
 (mermaid-mode :source "elpaca-menu-lock-file" :recipe
			   (:package "mermaid-mode" :repo "abrochard/mermaid-mode"
						 :fetcher github :files
						 ("*.el" "*.el.in" "dir" "*.info" "*.texi"
						  "*.texinfo" "doc/dir" "doc/*.info"
						  "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
						  "docs/dir" "docs/*.info" "docs/*.texi"
						  "docs/*.texinfo"
						  (:exclude ".dir-locals.el" "test.el"
									"tests.el" "*-test.el"
									"*-tests.el" "LICENSE" "README*"
									"*-pkg.el"))
						 :source "elpaca-menu-lock-file" :id
						 mermaid-mode :branch "master" :type git
						 :protocol https :inherit t :depth treeless
						 :ref
						 "804dbcb1452e7496ae0c48e20362da6351227246"))
 (move-text :source "elpaca-menu-lock-file" :recipe
			(:package "move-text" :fetcher github :repo
					  "emacsfodder/move-text" :files
					  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
					   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					   "doc/*.texinfo" "lisp/*.el" "docs/dir"
					   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					   (:exclude ".dir-locals.el" "test.el" "tests.el"
								 "*-test.el" "*-tests.el" "LICENSE"
								 "README*" "*-pkg.el"))
					  :source "elpaca-menu-lock-file" :id move-text
					  :branch "master" :type git :protocol https
					  :inherit t :depth treeless :ref
					  "142890cfb46d9c374113b4b49021a4202033147b"))
 (multi-vterm :source "elpaca-menu-lock-file" :recipe
			  (:package "multi-vterm" :fetcher github :repo
						"suonlight/multi-vterm" :files
						("*.el" "*.el.in" "dir" "*.info" "*.texi"
						 "*.texinfo" "doc/dir" "doc/*.info"
						 "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
						 "docs/dir" "docs/*.info" "docs/*.texi"
						 "docs/*.texinfo"
						 (:exclude ".dir-locals.el" "test.el"
								   "tests.el" "*-test.el" "*-tests.el"
								   "LICENSE" "README*" "*-pkg.el"))
						:source "elpaca-menu-lock-file" :id
						multi-vterm :branch "master" :type git
						:protocol https :inherit t :depth treeless
						:ref
						"36746d85870dac5aaee6b9af4aa1c3c0ef21a905"))
 (multiple-cursors :source "elpaca-menu-lock-file" :recipe
				   (:package "multiple-cursors" :fetcher github :repo
							 "magnars/multiple-cursors.el" :files
							 ("*.el" "*.el.in" "dir" "*.info" "*.texi"
							  "*.texinfo" "doc/dir" "doc/*.info"
							  "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
							  "docs/dir" "docs/*.info" "docs/*.texi"
							  "docs/*.texinfo"
							  (:exclude ".dir-locals.el" "test.el"
										"tests.el" "*-test.el"
										"*-tests.el" "LICENSE"
										"README*" "*-pkg.el"))
							 :source "elpaca-menu-lock-file" :id
							 multiple-cursors :branch "master" :type
							 git :protocol https :inherit t :depth
							 treeless :ref
							 "94b8b07a4bab87f803123723b68227565429dfa1"))
 (nix-ts-mode :source "elpaca-menu-lock-file" :recipe
			  (:package "nix-ts-mode" :fetcher github :repo
						"nix-community/nix-ts-mode" :files
						("*.el" "*.el.in" "dir" "*.info" "*.texi"
						 "*.texinfo" "doc/dir" "doc/*.info"
						 "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
						 "docs/dir" "docs/*.info" "docs/*.texi"
						 "docs/*.texinfo"
						 (:exclude ".dir-locals.el" "test.el"
								   "tests.el" "*-test.el" "*-tests.el"
								   "LICENSE" "README*" "*-pkg.el"))
						:source "elpaca-menu-lock-file" :id
						nix-ts-mode :branch "trunk" :type git
						:protocol https :inherit t :depth treeless
						:ref
						"3b69aeb8d2403ea52b60a46fa452965b7b37a50e"))
 (nov :source "elpaca-menu-lock-file" :recipe
	  (:package "nov" :fetcher git :url
				"https://depp.brause.cc/nov.el.git" :files
				("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
				 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
				 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
				 "docs/*.texinfo"
				 (:exclude ".dir-locals.el" "test.el" "tests.el"
						   "*-test.el" "*-tests.el" "LICENSE"
						   "README*" "*-pkg.el"))
				:source "elpaca-menu-lock-file" :id nov :branch
				"master" :type git :protocol https :inherit t :depth
				treeless :ref
				"874daf5e4791a6d4f47741422c80e2736e907351"))
 (ob-mermaid :source "elpaca-menu-lock-file" :recipe
			 (:package "ob-mermaid" :repo "arnm/ob-mermaid" :fetcher
					   github :files
					   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
						"*.texinfo" "doc/dir" "doc/*.info"
						"doc/*.texi" "doc/*.texinfo" "lisp/*.el"
						"docs/dir" "docs/*.info" "docs/*.texi"
						"docs/*.texinfo"
						(:exclude ".dir-locals.el" "test.el"
								  "tests.el" "*-test.el" "*-tests.el"
								  "LICENSE" "README*" "*-pkg.el"))
					   :source "elpaca-menu-lock-file" :id ob-mermaid
					   :branch "master" :type git :protocol https
					   :inherit t :depth treeless :ref
					   "30c2da02e3d24dbec0d004d3c6dfe7b381500b05"))
 (occult :source "elpaca-menu-lock-file" :recipe
		 (:package "occult" :fetcher github :repo "agzam/occult.el"
				   :files
				   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
					"*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					"doc/*.texinfo" "lisp/*.el" "docs/dir"
					"docs/*.info" "docs/*.texi" "docs/*.texinfo"
					(:exclude ".dir-locals.el" "test.el" "tests.el"
							  "*-test.el" "*-tests.el" "LICENSE"
							  "README*" "*-pkg.el"))
				   :source "elpaca-menu-lock-file" :id occult :branch
				   "main" :type git :protocol https :inherit t :depth
				   treeless :ref
				   "74006325af1d96af3f26dc2dbf02baccc03d83f1"))
 (operate-on-number :source "elpaca-menu-lock-file" :recipe
					(:package "operate-on-number" :fetcher github
							  :repo "knu/operate-on-number.el" :files
							  ("*.el" "*.el.in" "dir" "*.info"
							   "*.texi" "*.texinfo" "doc/dir"
							   "doc/*.info" "doc/*.texi"
							   "doc/*.texinfo" "lisp/*.el" "docs/dir"
							   "docs/*.info" "docs/*.texi"
							   "docs/*.texinfo"
							   (:exclude ".dir-locals.el" "test.el"
										 "tests.el" "*-test.el"
										 "*-tests.el" "LICENSE"
										 "README*" "*-pkg.el"))
							  :source "elpaca-menu-lock-file" :id
							  operate-on-number :branch "master" :type
							  git :protocol https :inherit t :depth
							  treeless :ref
							  "0ddebae1885c1b54eae1d79e66204d6d83c5065b"))
 (orderless :source "elpaca-menu-lock-file" :recipe
			(:package "orderless" :repo "oantolin/orderless" :fetcher
					  github :files
					  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
					   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					   "doc/*.texinfo" "lisp/*.el" "docs/dir"
					   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					   (:exclude ".dir-locals.el" "test.el" "tests.el"
								 "*-test.el" "*-tests.el" "LICENSE"
								 "README*" "*-pkg.el"))
					  :source "elpaca-menu-lock-file" :id orderless
					  :branch "master" :type git :protocol https
					  :inherit t :depth treeless :ref
					  "0ffd9d6903714c1f6d8fcbb6a20941fb33dd7ae5"))
 (org-bookmark-heading :source "elpaca-menu-lock-file" :recipe
					   (:package "org-bookmark-heading" :fetcher
								 github :repo
								 "alphapapa/org-bookmark-heading"
								 :files
								 ("*.el" "*.el.in" "dir" "*.info"
								  "*.texi" "*.texinfo" "doc/dir"
								  "doc/*.info" "doc/*.texi"
								  "doc/*.texinfo" "lisp/*.el"
								  "docs/dir" "docs/*.info"
								  "docs/*.texi" "docs/*.texinfo"
								  (:exclude ".dir-locals.el" "test.el"
											"tests.el" "*-test.el"
											"*-tests.el" "LICENSE"
											"README*" "*-pkg.el"))
								 :source "elpaca-menu-lock-file" :id
								 org-bookmark-heading :branch "master"
								 :type git :protocol https :inherit t
								 :depth treeless :ref
								 "bcab006ec42d7e2c92875c7170df193de2ee55f5"))
 (org-present :source "elpaca-menu-lock-file" :recipe
			  (:package "org-present" :fetcher github :repo
						"rlister/org-present" :files
						("*.el" "*.el.in" "dir" "*.info" "*.texi"
						 "*.texinfo" "doc/dir" "doc/*.info"
						 "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
						 "docs/dir" "docs/*.info" "docs/*.texi"
						 "docs/*.texinfo"
						 (:exclude ".dir-locals.el" "test.el"
								   "tests.el" "*-test.el" "*-tests.el"
								   "LICENSE" "README*" "*-pkg.el"))
						:source "elpaca-menu-lock-file" :id
						org-present :branch "master" :type git
						:protocol https :inherit t :depth treeless
						:ref
						"4ec04e1b77dea76d7c30066ccf3200d2e0b7bee9"))
 (org-sticky-header :source "elpaca-menu-lock-file" :recipe
					(:package "org-sticky-header" :fetcher github
							  :repo "alphapapa/org-sticky-header"
							  :files
							  ("*.el" "*.el.in" "dir" "*.info"
							   "*.texi" "*.texinfo" "doc/dir"
							   "doc/*.info" "doc/*.texi"
							   "doc/*.texinfo" "lisp/*.el" "docs/dir"
							   "docs/*.info" "docs/*.texi"
							   "docs/*.texinfo"
							   (:exclude ".dir-locals.el" "test.el"
										 "tests.el" "*-test.el"
										 "*-tests.el" "LICENSE"
										 "README*" "*-pkg.el"))
							  :source "elpaca-menu-lock-file" :id
							  org-sticky-header :branch "master" :type
							  git :protocol https :inherit t :depth
							  treeless :ref
							  "697875935b04b25c8229b9155a1ea0cab3ebe629"))
 (org-web-tools :source "elpaca-menu-lock-file" :recipe
				(:package "org-web-tools" :fetcher github :repo
						  "alphapapa/org-web-tools" :files
						  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
						   "*.texinfo" "doc/dir" "doc/*.info"
						   "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
						   "docs/dir" "docs/*.info" "docs/*.texi"
						   "docs/*.texinfo"
						   (:exclude ".dir-locals.el" "test.el"
									 "tests.el" "*-test.el"
									 "*-tests.el" "LICENSE" "README*"
									 "*-pkg.el"))
						  :source "elpaca-menu-lock-file" :id
						  org-web-tools :branch "master" :type git
						  :protocol https :inherit t :depth treeless
						  :ref
						  "7a6498f442fc7f29504745649948635c7165d847"))
 (orgalist :source "elpaca-menu-lock-file" :recipe
		   (:package "orgalist" :repo
					 ("https://github.com/emacsmirror/gnu_elpa"
					  . "orgalist")
					 :tar "1.16" :host gnu :branch
					 "externals/orgalist" :files
					 ("*" (:exclude ".git")) :source
					 "elpaca-menu-lock-file" :id orgalist :type git
					 :protocol https :inherit t :depth treeless :ref
					 "1f540f645eeb338019750d4dca3c056a1f5eca6d"))
 (osm :source "elpaca-menu-lock-file" :recipe
	  (:package "osm" :repo "minad/osm" :fetcher github :files
				("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
				 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
				 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
				 "docs/*.texinfo"
				 (:exclude ".dir-locals.el" "test.el" "tests.el"
						   "*-test.el" "*-tests.el" "LICENSE"
						   "README*" "*-pkg.el"))
				:source "elpaca-menu-lock-file" :id osm :branch "main"
				:type git :protocol https :inherit t :depth treeless
				:ref "d6167ca4f99bead657ac6f7f163c1fcf9b34267f"))
 (ox-asciidoc :source "elpaca-menu-lock-file" :recipe
			  (:package "ox-asciidoc" :fetcher github :repo
						"yashi/org-asciidoc" :files ("ox-asciidoc.el")
						:source "elpaca-menu-lock-file" :id
						ox-asciidoc :branch "master" :type git
						:protocol https :inherit t :depth treeless
						:ref
						"6836f452a3c77b9f4aa5d9835ea0a7a396326f7b"))
 (ox-gfm :source "elpaca-menu-lock-file" :recipe
		 (:package "ox-gfm" :fetcher github :repo "larstvei/ox-gfm"
				   :files
				   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
					"*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					"doc/*.texinfo" "lisp/*.el" "docs/dir"
					"docs/*.info" "docs/*.texi" "docs/*.texinfo"
					(:exclude ".dir-locals.el" "test.el" "tests.el"
							  "*-test.el" "*-tests.el" "LICENSE"
							  "README*" "*-pkg.el"))
				   :source "elpaca-menu-lock-file" :id ox-gfm :branch
				   "master" :type git :protocol https :inherit t
				   :depth treeless :ref
				   "4f774f13d34b3db9ea4ddb0b1edc070b1526ccbb"))
 (ox-jira :source "elpaca-menu-lock-file" :recipe
		  (:package "ox-jira" :repo "stig/ox-jira.el" :fetcher github
					:files
					("*.el" "*.el.in" "dir" "*.info" "*.texi"
					 "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					 "doc/*.texinfo" "lisp/*.el" "docs/dir"
					 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					 (:exclude ".dir-locals.el" "test.el" "tests.el"
							   "*-test.el" "*-tests.el" "LICENSE"
							   "README*" "*-pkg.el"))
					:source "elpaca-menu-lock-file" :id ox-jira
					:branch "trunk" :type git :protocol https :inherit
					t :depth treeless :ref
					"8748a908ee366e52539b19b863935d2ef7ebd0bf"))
 (package-lint :source "elpaca-menu-lock-file" :recipe
			   (:package "package-lint" :fetcher github :repo
						 "purcell/package-lint" :files
						 (:defaults "data" (:exclude "*flymake.el"))
						 :source "elpaca-menu-lock-file" :id
						 package-lint :branch "main" :type git
						 :protocol https :inherit t :depth treeless
						 :ref
						 "35996f478d81e51dae4fa30d051f741895d07399"))
 (parent-mode :source "elpaca-menu-lock-file" :recipe
			  (:package "parent-mode" :fetcher github :repo
						"Fanael/parent-mode" :files
						("*.el" "*.el.in" "dir" "*.info" "*.texi"
						 "*.texinfo" "doc/dir" "doc/*.info"
						 "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
						 "docs/dir" "docs/*.info" "docs/*.texi"
						 "docs/*.texinfo"
						 (:exclude ".dir-locals.el" "test.el"
								   "tests.el" "*-test.el" "*-tests.el"
								   "LICENSE" "README*" "*-pkg.el"))
						:source "elpaca-menu-lock-file" :id
						parent-mode :type git :protocol https :inherit
						t :depth treeless :ref
						"fbd49857ab2e4cd0c5611c0cc83f93711657b298"))
 (pass :source "elpaca-menu-lock-file" :recipe
	   (:package "pass" :fetcher github :repo "NicolasPetton/pass"
				 :files
				 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
				  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
				  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
				  "docs/*.texinfo"
				  (:exclude ".dir-locals.el" "test.el" "tests.el"
							"*-test.el" "*-tests.el" "LICENSE"
							"README*" "*-pkg.el"))
				 :source "elpaca-menu-lock-file" :id pass :branch
				 "master" :type git :protocol https :inherit t :depth
				 treeless :ref
				 "143456809fd2dbece9f241f4361085e1de0b0e75"))
 (password-store :source "elpaca-menu-lock-file" :recipe
				 (:package "password-store" :fetcher github :repo
						   "zx2c4/password-store" :files
						   ("contrib/emacs/*.el") :source
						   "elpaca-menu-lock-file" :id password-store
						   :branch "master" :type git :protocol https
						   :inherit t :depth treeless :ref
						   "3ca13cd8882cae4083c1c478858adbf2e82dd037"))
 (password-store-otp :source "elpaca-menu-lock-file" :recipe
					 (:package "password-store-otp" :repo
							   "volrath/password-store-otp.el"
							   :fetcher github :files
							   ("*.el" "*.el.in" "dir" "*.info"
								"*.texi" "*.texinfo" "doc/dir"
								"doc/*.info" "doc/*.texi"
								"doc/*.texinfo" "lisp/*.el" "docs/dir"
								"docs/*.info" "docs/*.texi"
								"docs/*.texinfo"
								(:exclude ".dir-locals.el" "test.el"
										  "tests.el" "*-test.el"
										  "*-tests.el" "LICENSE"
										  "README*" "*-pkg.el"))
							   :source "elpaca-menu-lock-file" :id
							   password-store-otp :protocol ssh
							   :remotes
							   (("fork" :repo
								 "nicolaisingh/password-store-otp.el"))
							   :branch "master" :type git :inherit t
							   :depth treeless :ref
							   "12e0e6ba0d4bef485153f05d2916298cd1a4ef7d"))
 (pcre2el :source "elpaca-menu-lock-file" :recipe
		  (:package "pcre2el" :fetcher github :repo "joddie/pcre2el"
					:files
					("*.el" "*.el.in" "dir" "*.info" "*.texi"
					 "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					 "doc/*.texinfo" "lisp/*.el" "docs/dir"
					 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					 (:exclude ".dir-locals.el" "test.el" "tests.el"
							   "*-test.el" "*-tests.el" "LICENSE"
							   "README*" "*-pkg.el"))
					:source "elpaca-menu-lock-file" :id pcre2el :type
					git :protocol https :inherit t :depth treeless
					:ref "b4d846d80dddb313042131cf2b8fbf647567e000"))
 (pdf-tools :source "elpaca-menu-lock-file" :recipe
			(:package "pdf-tools" :fetcher github :repo
					  "vedang/pdf-tools" :files
					  (:defaults "README" ("build" "Makefile")
								 ("build" "server"))
					  :source "elpaca-menu-lock-file" :id pdf-tools
					  :branch "master" :type git :protocol https
					  :inherit t :depth treeless :ref
					  "365f88238f46f9b1425685562105881800f10386"))
 (plantuml-mode :source "elpaca-menu-lock-file" :recipe
				(:package "plantuml-mode" :fetcher github :repo
						  "skuro/plantuml-mode" :files
						  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
						   "*.texinfo" "doc/dir" "doc/*.info"
						   "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
						   "docs/dir" "docs/*.info" "docs/*.texi"
						   "docs/*.texinfo"
						   (:exclude ".dir-locals.el" "test.el"
									 "tests.el" "*-test.el"
									 "*-tests.el" "LICENSE" "README*"
									 "*-pkg.el"))
						  :source "elpaca-menu-lock-file" :id
						  plantuml-mode :branch "develop" :type git
						  :protocol https :inherit t :depth treeless
						  :ref
						  "a4a63efa4a3980bfbd825bfb3a263c6664401e79"))
 (plz :source "elpaca-menu-lock-file" :recipe
	  (:package "plz" :repo
				("https://github.com/alphapapa/plz.el.git" . "plz")
				:tar "0.9.1" :host gnu :files
				("*" (:exclude ".git" "LICENSE")) :source
				"elpaca-menu-lock-file" :id plz :branch "master" :type
				git :protocol https :inherit t :depth treeless :ref
				"e2d07838e3b64ee5ebe59d4c3c9011adefb7b58e"))
 (prettier-js :source "elpaca-menu-lock-file" :recipe
			  (:package "prettier-js" :repo "prettier/prettier-emacs"
						:fetcher github :files
						("*.el" "*.el.in" "dir" "*.info" "*.texi"
						 "*.texinfo" "doc/dir" "doc/*.info"
						 "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
						 "docs/dir" "docs/*.info" "docs/*.texi"
						 "docs/*.texinfo"
						 (:exclude ".dir-locals.el" "test.el"
								   "tests.el" "*-test.el" "*-tests.el"
								   "LICENSE" "README*" "*-pkg.el"))
						:source "elpaca-menu-lock-file" :id
						prettier-js :branch "main" :type git :protocol
						https :inherit t :depth treeless :ref
						"29ea00ae63d2b45b5ae86a46a190924f2d589f2c"))
 (prism :source "elpaca-menu-lock-file" :recipe
		(:package "prism" :fetcher github :repo "alphapapa/prism.el"
				  :files
				  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
				   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
				   "doc/*.texinfo" "lisp/*.el" "docs/dir"
				   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
				   (:exclude ".dir-locals.el" "test.el" "tests.el"
							 "*-test.el" "*-tests.el" "LICENSE"
							 "README*" "*-pkg.el"))
				  :source "elpaca-menu-lock-file" :id prism :branch
				  "master" :type git :protocol https :inherit t :depth
				  treeless :ref
				  "2fa8eb5a9ca62a548d33befef4517e5d0266eb28"))
 (prodigy :source "elpaca-menu-lock-file" :recipe
		  (:package "prodigy" :fetcher github :repo
					"rejeep/prodigy.el" :files
					("*.el" "*.el.in" "dir" "*.info" "*.texi"
					 "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					 "doc/*.texinfo" "lisp/*.el" "docs/dir"
					 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					 (:exclude ".dir-locals.el" "test.el" "tests.el"
							   "*-test.el" "*-tests.el" "LICENSE"
							   "README*" "*-pkg.el"))
					:source "elpaca-menu-lock-file" :id prodigy
					:branch "master" :type git :protocol https
					:inherit t :depth treeless :ref
					"336450ba90864dec62c8c883554d34aa18132165"))
 (protobuf-mode :source "elpaca-menu-lock-file" :recipe
				(:package "protobuf-mode" :fetcher github :repo
						  "protocolbuffers/protobuf" :files
						  ("editors/protobuf-mode.el") :source
						  "elpaca-menu-lock-file" :id protobuf-mode
						  :branch "main" :type git :protocol https
						  :inherit t :depth treeless :ref
						  "7a39a4e3136697bc1f87f55b79767a55d7aeaba3"))
 (pytest :source "elpaca-menu-lock-file" :recipe
		 (:package "pytest" :fetcher github :repo "ionrock/pytest-el"
				   :files
				   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
					"*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					"doc/*.texinfo" "lisp/*.el" "docs/dir"
					"docs/*.info" "docs/*.texi" "docs/*.texinfo"
					(:exclude ".dir-locals.el" "test.el" "tests.el"
							  "*-test.el" "*-tests.el" "LICENSE"
							  "README*" "*-pkg.el"))
				   :source "elpaca-menu-lock-file" :id pytest :branch
				   "master" :type git :protocol https :inherit t
				   :depth treeless :ref
				   "8692f965bf4ddf3d755cf1fbf77a7a768e22460e"))
 (pyvenv :source "elpaca-menu-lock-file" :recipe
		 (:package "pyvenv" :fetcher github :repo
				   "jorgenschaefer/pyvenv" :files
				   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
					"*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					"doc/*.texinfo" "lisp/*.el" "docs/dir"
					"docs/*.info" "docs/*.texi" "docs/*.texinfo"
					(:exclude ".dir-locals.el" "test.el" "tests.el"
							  "*-test.el" "*-tests.el" "LICENSE"
							  "README*" "*-pkg.el"))
				   :source "elpaca-menu-lock-file" :id pyvenv :branch
				   "master" :type git :protocol https :inherit t
				   :depth treeless :ref
				   "31ea715f2164dd611e7fc77b26390ef3ca93509b"))
 (qrencode :source "elpaca-menu-lock-file" :recipe
		   (:package "qrencode" :fetcher github :repo
					 "ruediger/qrencode-el" :files
					 ("*.el" "*.el.in" "dir" "*.info" "*.texi"
					  "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					  "doc/*.texinfo" "lisp/*.el" "docs/dir"
					  "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					  (:exclude ".dir-locals.el" "test.el" "tests.el"
								"*-test.el" "*-tests.el" "LICENSE"
								"README*" "*-pkg.el"))
					 :source "elpaca-menu-lock-file" :id qrencode
					 :branch "main" :type git :protocol https :inherit
					 t :depth treeless :ref
					 "7df905c77f590278d301679db354a3d7662407c9"))
 (rainbow-mode :source "elpaca-menu-lock-file" :recipe
			   (:package "rainbow-mode" :repo
						 ("https://github.com/emacsmirror/gnu_elpa"
						  . "rainbow-mode")
						 :tar "1.0.6" :host gnu :branch
						 "externals/rainbow-mode" :files
						 ("*" (:exclude ".git")) :source
						 "elpaca-menu-lock-file" :id rainbow-mode
						 :type git :protocol https :inherit t :depth
						 treeless :ref
						 "f7db3b5919f70420a91eb199f8663468de3033f3"))
 (request :source "elpaca-menu-lock-file"
   :recipe
   (:package "request" :repo "tkf/emacs-request" :fetcher github
			 :files ("request.el") :source "elpaca-menu-lock-file" :id
			 request :type git :protocol https :inherit t :depth
			 treeless :ref "c22e3c23a6dd90f64be536e176ea0ed6113a5ba6"))
 (restclient :source "elpaca-menu-lock-file" :recipe
			 (:package "restclient" :fetcher github :repo
					   "emacsorphanage/restclient" :files
					   ("restclient.el") :source
					   "elpaca-menu-lock-file" :id restclient :branch
					   "master" :type git :protocol https :inherit t
					   :depth treeless :ref
					   "d280632df39a175dac06037038105e32945624be"))
 (s :source "elpaca-menu-lock-file" :recipe
	(:package "s" :fetcher github :repo "magnars/s.el" :files
			  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			   "docs/*.texinfo"
			   (:exclude ".dir-locals.el" "test.el" "tests.el"
						 "*-test.el" "*-tests.el" "LICENSE" "README*"
						 "*-pkg.el"))
			  :source "elpaca-menu-lock-file" :id s :type git
			  :protocol https :inherit t :depth treeless :ref
			  "7393fa6fa305403e628058c0ec78c35d610fab05"))
 (saveplace-pdf-view :source "elpaca-menu-lock-file" :recipe
					 (:package "saveplace-pdf-view" :fetcher github
							   :repo "nicolaisingh/saveplace-pdf-view"
							   :files
							   ("*.el" "*.el.in" "dir" "*.info"
								"*.texi" "*.texinfo" "doc/dir"
								"doc/*.info" "doc/*.texi"
								"doc/*.texinfo" "lisp/*.el" "docs/dir"
								"docs/*.info" "docs/*.texi"
								"docs/*.texinfo"
								(:exclude ".dir-locals.el" "test.el"
										  "tests.el" "*-test.el"
										  "*-tests.el" "LICENSE"
										  "README*" "*-pkg.el"))
							   :source "elpaca-menu-lock-file" :id
							   saveplace-pdf-view :protocol ssh
							   :branch "master" :type git :inherit t
							   :depth treeless :ref
							   "dc1e0b28a5ed8319a0b6725abaffba7c2fa8c730"))
 (selected :source "elpaca-menu-lock-file" :recipe
		   (:package "selected" :repo "Kungsgeten/selected.el"
					 :fetcher github :files
					 ("*.el" "*.el.in" "dir" "*.info" "*.texi"
					  "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					  "doc/*.texinfo" "lisp/*.el" "docs/dir"
					  "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					  (:exclude ".dir-locals.el" "test.el" "tests.el"
								"*-test.el" "*-tests.el" "LICENSE"
								"README*" "*-pkg.el"))
					 :source "elpaca-menu-lock-file" :id selected
					 :branch "master" :type git :protocol https
					 :inherit t :depth treeless :ref
					 "9f5a6324e4911515972989841f143696472f2374"))
 (shell-maker :source "elpaca-menu-lock-file" :recipe
			  (:package "shell-maker" :fetcher github :repo
						"xenodium/shell-maker" :files
						("*.el" "*.el.in" "dir" "*.info" "*.texi"
						 "*.texinfo" "doc/dir" "doc/*.info"
						 "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
						 "docs/dir" "docs/*.info" "docs/*.texi"
						 "docs/*.texinfo"
						 (:exclude ".dir-locals.el" "test.el"
								   "tests.el" "*-test.el" "*-tests.el"
								   "LICENSE" "README*" "*-pkg.el"))
						:source "elpaca-menu-lock-file" :id
						shell-maker :branch "main" :type git :protocol
						https :inherit t :depth treeless :ref
						"55607423f052c6be72b1a5da394427c9faaa413d"))
 (slime :source "elpaca-menu-lock-file" :recipe
		(:package "slime" :fetcher github :repo "slime/slime" :files
				  ("*.el" ("lib" "lib/hyperspec.el") "swank" "*.lisp"
				   "*.asd" "doc/slime.texi" "doc/slime.info" "doc/dir"
				   "ChangeLog" ("contrib" "contrib/*")
				   (:exclude "contrib/test" "contrib/Makefile"))
				  :source "elpaca-menu-lock-file" :id slime :branch
				  "master" :type git :protocol https :inherit t :depth
				  treeless :ref
				  "883b19c77ca42f827b5e4631c5428e244c2e063c"))
 (smartparens :source "elpaca-menu-lock-file" :recipe
			  (:package "smartparens" :fetcher github :repo
						"Fuco1/smartparens" :files
						("*.el" "*.el.in" "dir" "*.info" "*.texi"
						 "*.texinfo" "doc/dir" "doc/*.info"
						 "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
						 "docs/dir" "docs/*.info" "docs/*.texi"
						 "docs/*.texinfo"
						 (:exclude ".dir-locals.el" "test.el"
								   "tests.el" "*-test.el" "*-tests.el"
								   "LICENSE" "README*" "*-pkg.el"))
						:source "elpaca-menu-lock-file" :id
						smartparens :branch "master" :type git
						:protocol https :inherit t :depth treeless
						:ref
						"82d2cf084a19b0c2c3812e0550721f8a61996056"))
 (spacious-padding :source "elpaca-menu-lock-file" :recipe
				   (:package "spacious-padding" :repo
							 ("https://github.com/protesilaos/spacious-padding"
							  . "spacious-padding")
							 :tar "0.8.0" :host gnu :files
							 ("*"
							  (:exclude ".git" "COPYING"
										"doclicense.texi"))
							 :source "elpaca-menu-lock-file" :id
							 spacious-padding :branch "main" :type git
							 :protocol https :inherit t :depth
							 treeless :ref
							 "0fdbfdbd61a3ad487c776317787c1f8c2148e8fe"))
 (spinner :source "elpaca-menu-lock-file" :recipe
		  (:package "spinner" :repo
					("https://github.com/Malabarba/spinner.el"
					 . "spinner")
					:tar "1.7.4" :host gnu :files
					("*" (:exclude ".git")) :source
					"elpaca-menu-lock-file" :id spinner :type git
					:protocol https :inherit t :depth treeless :ref
					"d4647ae87fb0cd24bc9081a3d287c860ff061c21"))
 (symbol-overlay :source "elpaca-menu-lock-file" :recipe
				 (:package "symbol-overlay" :fetcher github :repo
						   "wolray/symbol-overlay" :files
						   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
							"*.texinfo" "doc/dir" "doc/*.info"
							"doc/*.texi" "doc/*.texinfo" "lisp/*.el"
							"docs/dir" "docs/*.info" "docs/*.texi"
							"docs/*.texinfo"
							(:exclude ".dir-locals.el" "test.el"
									  "tests.el" "*-test.el"
									  "*-tests.el" "LICENSE" "README*"
									  "*-pkg.el"))
						   :source "elpaca-menu-lock-file" :id
						   symbol-overlay :branch "master" :type git
						   :protocol https :inherit t :depth treeless
						   :ref
						   "85d100b0cca35b70cee1b260e09af8e1fb2fcc08"))
 (tablist :source "elpaca-menu-lock-file" :recipe
		  (:package "tablist" :fetcher github :repo
					"emacsorphanage/tablist" :files
					("*.el" "*.el.in" "dir" "*.info" "*.texi"
					 "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					 "doc/*.texinfo" "lisp/*.el" "docs/dir"
					 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					 (:exclude ".dir-locals.el" "test.el" "tests.el"
							   "*-test.el" "*-tests.el" "LICENSE"
							   "README*" "*-pkg.el"))
					:source "elpaca-menu-lock-file" :id tablist :type
					git :protocol https :inherit t :depth treeless
					:ref "01f065e387ffe6b7a41f180f257cd12551c7a9c2"))
 (terraform-mode :source "elpaca-menu-lock-file" :recipe
				 (:package "terraform-mode" :repo
						   "hcl-emacs/terraform-mode" :fetcher github
						   :files
						   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
							"*.texinfo" "doc/dir" "doc/*.info"
							"doc/*.texi" "doc/*.texinfo" "lisp/*.el"
							"docs/dir" "docs/*.info" "docs/*.texi"
							"docs/*.texinfo"
							(:exclude ".dir-locals.el" "test.el"
									  "tests.el" "*-test.el"
									  "*-tests.el" "LICENSE" "README*"
									  "*-pkg.el"))
						   :source "elpaca-menu-lock-file" :id
						   terraform-mode :branch "master" :type git
						   :protocol https :inherit t :depth treeless
						   :ref
						   "01635df3625c0cec2bb4613a6f920b8569d41009"))
 (transient :source "elpaca-menu-lock-file" :recipe
			(:package "transient" :fetcher github :repo
					  "magit/transient" :files
					  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
					   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					   "doc/*.texinfo" "lisp/*.el" "docs/dir"
					   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					   (:exclude ".dir-locals.el" "test.el" "tests.el"
								 "*-test.el" "*-tests.el" "LICENSE"
								 "README*" "*-pkg.el"))
					  :source "elpaca-menu-lock-file" :id transient
					  :branch "main" :type git :protocol https
					  :inherit t :depth treeless :ref
					  "3d20a780605f0a33d6360dc0a2ce9174c69a9a92"))
 (tree-mode :source "elpaca-menu-lock-file" :recipe
			(:package "tree-mode" :fetcher github :repo
					  "emacsorphanage/tree-mode" :files
					  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
					   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					   "doc/*.texinfo" "lisp/*.el" "docs/dir"
					   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					   (:exclude ".dir-locals.el" "test.el" "tests.el"
								 "*-test.el" "*-tests.el" "LICENSE"
								 "README*" "*-pkg.el"))
					  :source "elpaca-menu-lock-file" :id tree-mode
					  :branch "master" :type git :protocol https
					  :inherit t :depth treeless :ref
					  "b06078826d5875d74b0e7b7ac47b0d0917610534"))
 (treepy :source "elpaca-menu-lock-file" :recipe
		 (:package "treepy" :repo "volrath/treepy.el" :fetcher github
				   :files
				   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
					"*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					"doc/*.texinfo" "lisp/*.el" "docs/dir"
					"docs/*.info" "docs/*.texi" "docs/*.texinfo"
					(:exclude ".dir-locals.el" "test.el" "tests.el"
							  "*-test.el" "*-tests.el" "LICENSE"
							  "README*" "*-pkg.el"))
				   :source "elpaca-menu-lock-file" :id treepy :type
				   git :protocol https :inherit t :depth treeless :ref
				   "806c000bd40153d17dfa5709c6d19546d507a416"))
 (treesit-fold :source "elpaca-menu-lock-file" :recipe
			   (:package "treesit-fold" :repo
						 ("https://github.com/emacs-tree-sitter/treesit-fold"
						  . "treesit-fold")
						 :tar "0.2.1" :host nongnu :files
						 ("*" (:exclude ".git")) :source
						 "elpaca-menu-lock-file" :id treesit-fold
						 :branch "master" :type git :protocol https
						 :inherit t :depth treeless :ref
						 "cc1003b730a3167b972cc8400dffe19be7988fc7"))
 (unfill :source "elpaca-menu-lock-file" :recipe
		 (:package "unfill" :fetcher github :repo "purcell/unfill"
				   :files
				   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
					"*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					"doc/*.texinfo" "lisp/*.el" "docs/dir"
					"docs/*.info" "docs/*.texi" "docs/*.texinfo"
					(:exclude ".dir-locals.el" "test.el" "tests.el"
							  "*-test.el" "*-tests.el" "LICENSE"
							  "README*" "*-pkg.el"))
				   :source "elpaca-menu-lock-file" :id unfill :branch
				   "main" :type git :protocol https :inherit t :depth
				   treeless :ref
				   "4a15511876983eeaa75e57fcab8d4d51fe9b3840"))
 (uniline :source "elpaca-menu-lock-file" :recipe
		  (:package "uniline" :fetcher github :repo "tbanel/uniline"
					:files
					("*.el" "*.el.in" "dir" "*.info" "*.texi"
					 "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					 "doc/*.texinfo" "lisp/*.el" "docs/dir"
					 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					 (:exclude ".dir-locals.el" "test.el" "tests.el"
							   "*-test.el" "*-tests.el" "LICENSE"
							   "README*" "*-pkg.el"))
					:source "elpaca-menu-lock-file" :id uniline
					:branch "main" :type git :protocol https :inherit
					t :depth treeless :ref
					"2d58c8cf237dfc0e7408f1e62e2dbfaf0dd9e99e"))
 (uuidgen :source "elpaca-menu-lock-file" :recipe
		  (:package "uuidgen" :fetcher github :repo "kanru/uuidgen-el"
					:files
					("*.el" "*.el.in" "dir" "*.info" "*.texi"
					 "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					 "doc/*.texinfo" "lisp/*.el" "docs/dir"
					 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					 (:exclude ".dir-locals.el" "test.el" "tests.el"
							   "*-test.el" "*-tests.el" "LICENSE"
							   "README*" "*-pkg.el"))
					:source "elpaca-menu-lock-file" :id uuidgen
					:branch "master" :type git :protocol https
					:inherit t :depth treeless :ref
					"cebbe09d27c63abe61fe8c2e2248587d90265b59"))
 (visual-fill-column :source "elpaca-menu-lock-file" :recipe
					 (:package "visual-fill-column" :fetcher codeberg
							   :repo "joostkremers/visual-fill-column"
							   :files
							   ("*.el" "*.el.in" "dir" "*.info"
								"*.texi" "*.texinfo" "doc/dir"
								"doc/*.info" "doc/*.texi"
								"doc/*.texinfo" "lisp/*.el" "docs/dir"
								"docs/*.info" "docs/*.texi"
								"docs/*.texinfo"
								(:exclude ".dir-locals.el" "test.el"
										  "tests.el" "*-test.el"
										  "*-tests.el" "LICENSE"
										  "README*" "*-pkg.el"))
							   :source "elpaca-menu-lock-file" :id
							   visual-fill-column :branch "main" :type
							   git :protocol https :inherit t :depth
							   treeless :ref
							   "e1be9a1545157d24454d950c0ac79553c540edb7"))
 (vterm :source "elpaca-menu-lock-file" :recipe
		(:package "vterm" :fetcher github :repo
				  "akermu/emacs-libvterm" :files
				  ("CMakeLists.txt" "elisp.c" "elisp.h"
				   "emacs-module.h" "etc" "utf8.c" "utf8.h" "vterm.el"
				   "vterm-module.c" "vterm-module.h")
				  :source "elpaca-menu-lock-file" :id vterm :branch
				  "master" :type git :protocol https :inherit t :depth
				  treeless :ref
				  "9a32a4afce25647282bc8a8792468e41bc64adf4"))
 (vundo :source "elpaca-menu-lock-file" :recipe
		(:package "vundo" :repo
				  ("https://github.com/casouri/vundo" . "vundo") :tar
				  "2.4.0" :host gnu :files
				  ("*" (:exclude ".git" "test")) :source
				  "elpaca-menu-lock-file" :id vundo :branch "master"
				  :type git :protocol https :inherit t :depth treeless
				  :ref "e0af8c5845abf884a644215a9cac37f39c13cd5a"))
 (web-server :source "elpaca-menu-lock-file" :recipe
			 (:package "web-server" :fetcher github :repo
					   "eschulte/emacs-web-server" :files
					   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
						"*.texinfo" "doc/dir" "doc/*.info"
						"doc/*.texi" "doc/*.texinfo" "lisp/*.el"
						"docs/dir" "docs/*.info" "docs/*.texi"
						"docs/*.texinfo"
						(:exclude ".dir-locals.el" "test.el"
								  "tests.el" "*-test.el" "*-tests.el"
								  "LICENSE" "README*" "*-pkg.el"))
					   :source "elpaca-menu-lock-file" :id web-server
					   :type git :protocol https :inherit t :depth
					   treeless :ref
					   "6357a1c2d1718778503f7ee0909585094117525b"))
 (websocket :source "elpaca-menu-lock-file" :recipe
			(:package "websocket" :repo "ahyatt/emacs-websocket"
					  :fetcher github :files
					  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
					   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					   "doc/*.texinfo" "lisp/*.el" "docs/dir"
					   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					   (:exclude ".dir-locals.el" "test.el" "tests.el"
								 "*-test.el" "*-tests.el" "LICENSE"
								 "README*" "*-pkg.el"))
					  :source "elpaca-menu-lock-file" :id websocket
					  :type git :protocol https :inherit t :depth
					  treeless :ref
					  "2195e1247ecb04c30321702aa5f5618a51c329c5"))
 (whisper :source "elpaca-menu-lock-file" :recipe
		  (:source "elpaca-menu-lock-file" :package "whisper" :id
				   whisper :host github :repo "natrys/whisper.el"
				   :branch "master" :type git :protocol https :inherit
				   t :depth treeless :ref
				   "fd9bf5787a99dd31a4bdf54d2bd9821aacf84e93"))
 (with-editor :source "elpaca-menu-lock-file"
   :recipe
   (:package "with-editor" :fetcher github :repo "magit/with-editor"
			 :files
			 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			  "docs/*.texinfo"
			  (:exclude ".dir-locals.el" "test.el" "tests.el"
						"*-test.el" "*-tests.el" "LICENSE" "README*"
						"*-pkg.el"))
			 :source "elpaca-menu-lock-file" :id with-editor :type git
			 :protocol https :inherit t :depth treeless :ref
			 "45bfc6084f03e3aa7f4f8db20836d559186c5957"))
 (writeroom-mode :source "elpaca-menu-lock-file" :recipe
				 (:package "writeroom-mode" :fetcher github :repo
						   "joostkremers/writeroom-mode" :files
						   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
							"*.texinfo" "doc/dir" "doc/*.info"
							"doc/*.texi" "doc/*.texinfo" "lisp/*.el"
							"docs/dir" "docs/*.info" "docs/*.texi"
							"docs/*.texinfo"
							(:exclude ".dir-locals.el" "test.el"
									  "tests.el" "*-test.el"
									  "*-tests.el" "LICENSE" "README*"
									  "*-pkg.el"))
						   :source "elpaca-menu-lock-file" :id
						   writeroom-mode :branch "master" :type git
						   :protocol https :inherit t :depth treeless
						   :ref
						   "cca2b4b3cfcfea1919e1870519d79ed1a69aa5e2"))
 (xterm-color :source "elpaca-menu-lock-file" :recipe
			  (:package "xterm-color" :repo "atomontage/xterm-color"
						:fetcher github :files
						("*.el" "*.el.in" "dir" "*.info" "*.texi"
						 "*.texinfo" "doc/dir" "doc/*.info"
						 "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
						 "docs/dir" "docs/*.info" "docs/*.texi"
						 "docs/*.texinfo"
						 (:exclude ".dir-locals.el" "test.el"
								   "tests.el" "*-test.el" "*-tests.el"
								   "LICENSE" "README*" "*-pkg.el"))
						:source "elpaca-menu-lock-file" :id
						xterm-color :type git :protocol https :inherit
						t :depth treeless :ref
						"ffdad85e584dfc0857f2a1fb970f5ef0f5d31ba3"))
 (yaml :source "elpaca-menu-lock-file" :recipe
	   (:package "yaml" :repo "zkry/yaml.el" :fetcher github :files
				 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
				  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
				  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
				  "docs/*.texinfo"
				  (:exclude ".dir-locals.el" "test.el" "tests.el"
							"*-test.el" "*-tests.el" "LICENSE"
							"README*" "*-pkg.el"))
				 :source "elpaca-menu-lock-file" :id yaml :type git
				 :protocol https :inherit t :depth treeless :ref
				 "5546f36bde24a9a8c1934e0f6ce205cd41d72537"))
 (yaml-pro :source "elpaca-menu-lock-file" :recipe
		   (:package "yaml-pro" :repo "zkry/yaml-pro" :fetcher github
					 :files
					 ("*.el" "*.el.in" "dir" "*.info" "*.texi"
					  "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					  "doc/*.texinfo" "lisp/*.el" "docs/dir"
					  "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					  (:exclude ".dir-locals.el" "test.el" "tests.el"
								"*-test.el" "*-tests.el" "LICENSE"
								"README*" "*-pkg.el"))
					 :source "elpaca-menu-lock-file" :id yaml-pro
					 :branch "master" :type git :protocol https
					 :inherit t :depth treeless :ref
					 "9b9509188e5b88bb933e98ab36ab992519b9554b"))
 (yasnippet :source "elpaca-menu-lock-file" :recipe
			(:package "yasnippet" :fetcher github :repo
					  "joaotavora/yasnippet" :files
					  (:defaults ("doc" "doc/*.org")) :source
					  "elpaca-menu-lock-file" :id yasnippet :branch
					  "master" :type git :protocol https :inherit t
					  :depth treeless :ref
					  "c1e6ff23e9af16b856c88dfaab9d3ad7b746ad37"))
 (ztree :source "elpaca-menu-lock-file" :recipe
		(:package "ztree" :fetcher codeberg :repo "fourier/ztree"
				  :files
				  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
				   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
				   "doc/*.texinfo" "lisp/*.el" "docs/dir"
				   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
				   (:exclude ".dir-locals.el" "test.el" "tests.el"
							 "*-test.el" "*-tests.el" "LICENSE"
							 "README*" "*-pkg.el"))
				  :source "elpaca-menu-lock-file" :id ztree :branch
				  "master" :type git :protocol https :inherit t :depth
				  treeless :ref
				  "9905f1be006fe02417fc6598be4990746053bbec")))
