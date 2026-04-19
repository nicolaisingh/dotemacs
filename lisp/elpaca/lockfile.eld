((acp :source "elpaca-menu-lock-file" :recipe
	  (:package "acp" :fetcher github :repo "xenodium/acp.el" :files
				("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
				 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
				 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
				 "docs/*.texinfo"
				 (:exclude ".dir-locals.el" "test.el" "tests.el"
						   "*-test.el" "*-tests.el" "LICENSE"
						   "README*" "*-pkg.el"))
				:source "elpaca-menu-lock-file" :id acp :type git
				:protocol https :inherit t :depth treeless :ref
				"c32fbf8df34ed0095853a8cf55dc783e68b67d90"))
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
						agent-shell :type git :protocol https :inherit
						t :depth treeless :ref
						"eb5a379c1db4e395a04acd1f16a6dfb92e803d60"))
 (agent-shell-sidebar :source "elpaca-menu-lock-file" :recipe
					  (:source "elpaca-menu-lock-file" :package
							   "agent-shell-sidebar" :id
							   agent-shell-sidebar :host github :repo
							   "cmacrae/agent-shell-sidebar" :type git
							   :protocol https :inherit t :depth
							   treeless :ref
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
							  :source "elpaca-menu-lock-file"
							  :protocol https :inherit t :depth
							  treeless :ref
							  "a437a45868f94b77362c6b913c5ee8e67b273c42"
							  :id aggressive-indent :type git))
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
				  :source "elpaca-menu-lock-file" :protocol https
				  :inherit t :depth treeless :ref
				  "79f6936ab4d85227530959811143429347a6971b" :id alert
				  :type git))
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
				  :source "elpaca-menu-lock-file" :protocol https
				  :inherit t :depth treeless :ref
				  "bb3f31966ed65a76abe6fa4f80a960a2917f554e" :id async
				  :type git))
 (avy :source "elpaca-menu-lock-file" :recipe
	  (:package "avy" :repo "abo-abo/avy" :fetcher github :files
				("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
				 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
				 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
				 "docs/*.texinfo"
				 (:exclude ".dir-locals.el" "test.el" "tests.el"
						   "*-test.el" "*-tests.el" "LICENSE"
						   "README*" "*-pkg.el"))
				:source "elpaca-menu-lock-file" :protocol https
				:inherit t :depth treeless :ref
				"933d1f36cca0f71e4acb5fac707e9ae26c536264" :id avy
				:type git))
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
					:source "elpaca-menu-lock-file" :protocol https
					:inherit t :depth treeless :ref
					"7d8dc2ae21257e91b7646a6f3bb97c8b8bd95434" :id
					blacken :type git))
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
							 :source "elpaca-menu-lock-file" :protocol
							 https :inherit t :depth treeless :ref
							 "03cc18c08a549568edb6bc710c307c19bc507ef3"
							 :id browse-kill-ring :type git))
 (calibre :source "elpaca-menu-lock-file" :recipe
		  (:package "calibre" :repo
					("https://git.disroot.org/kjartanoli/calibre.el"
					 . "calibre")
					:files ("*" (:exclude ".git")) :source
					"elpaca-menu-lock-file" :protocol https :inherit t
					:depth treeless :ref
					"855a9cb092eecf1e5c17ea56c440a0200f95eb4c" :id
					calibre :type git))
 (cape :source "elpaca-menu-lock-file" :recipe
	   (:package "cape" :repo "minad/cape" :fetcher github :files
				 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
				  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
				  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
				  "docs/*.texinfo"
				  (:exclude ".dir-locals.el" "test.el" "tests.el"
							"*-test.el" "*-tests.el" "LICENSE"
							"README*" "*-pkg.el"))
				 :source "elpaca-menu-lock-file" :protocol https
				 :inherit t :depth treeless :ref
				 "a9b78ac918df991716de3f84a95db91bb4df44b6" :id cape
				 :type git))
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
								 :source "elpaca-menu-lock-file"
								 :protocol https :inherit t :depth
								 treeless :ref
								 "67ef719e685407dbc455c7430765e4e685fd95a9"
								 :id centered-cursor-mode :type git))
 (chess :source "elpaca-menu-lock-file" :recipe
		(:package "chess" :repo
				  ("https://github.com/emacsmirror/gnu_elpa" . "chess")
				  :branch "externals/chess" :files
				  ("*" (:exclude ".git")) :source
				  "elpaca-menu-lock-file" :protocol https :inherit t
				  :depth treeless :ref
				  "e4adaf1a8e276ff556763aca8a47e10193f2bed4" :id chess
				  :type git))
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
					:source "elpaca-menu-lock-file" :protocol https
					:inherit t :depth treeless :ref
					"5ea0bf7c3881ea905e280446342539b242401979" :id
					chronos :type git))
 (claude-code-ide :source "elpaca-menu-lock-file" :recipe
				  (:source "elpaca-menu-lock-file" :package
						   "claude-code-ide" :id claude-code-ide :host
						   github :repo "manzaltu/claude-code-ide.el"
						   :type git :protocol https :inherit t :depth
						   treeless :ref
						   "56db02ee386d009ddb8b1482310f1f9beeefb810"))
 (clojure-mode :source "elpaca-menu-lock-file" :recipe
			   (:package "clojure-mode" :repo
						 "clojure-emacs/clojure-mode" :fetcher github
						 :files ("clojure-mode.el") :source
						 "elpaca-menu-lock-file" :protocol https
						 :inherit t :depth treeless :ref
						 "28dc02114ae70db6bb68d537ea77985f272120bc"
						 :id clojure-mode :type git))
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
				   :source "elpaca-menu-lock-file" :protocol https
				   :inherit t :depth treeless :ref
				   "05a2b048fd4e5c90aa971479cb9e71cf9aeba2bf" :id
				   closql :type git))
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
							 :source "elpaca-menu-lock-file" :protocol
							 https :inherit t :depth treeless :ref
							 "af600e6b4129c8115f464af576505ea8e789db27"
							 :id command-log-mode :type git))
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
					:source "elpaca-menu-lock-file" :protocol https
					:inherit t :depth treeless :ref
					"b6fd7feda74267a53f9f1cf1bd122e519a308132" :id
					consult :type git))
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
						:source "elpaca-menu-lock-file" :protocol
						https :inherit t :depth treeless :ref
						"4532b8d215d16b0159691ce4dee693e72d71e0ff" :id
						consult-dir :type git))
 (corfu :source "elpaca-menu-lock-file" :recipe
		(:package "corfu" :repo "minad/corfu" :files
				  (:defaults "extensions/corfu-*.el") :fetcher github
				  :source "elpaca-menu-lock-file" :protocol https
				  :inherit t :depth treeless :ref
				  "d6cc682c15b594f2bc9f55196a382a5fda7a7fc6" :id corfu
				  :type git))
 (cov :source "elpaca-menu-lock-file" :recipe
	  (:package "cov" :fetcher github :repo "AdamNiederer/cov" :files
				("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
				 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
				 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
				 "docs/*.texinfo"
				 (:exclude ".dir-locals.el" "test.el" "tests.el"
						   "*-test.el" "*-tests.el" "LICENSE"
						   "README*" "*-pkg.el"))
				:source "elpaca-menu-lock-file" :protocol https
				:inherit t :depth treeless :ref
				"7a3599e42d4fe943b912701e04beffcf2ec812d2" :id cov
				:type git))
 (csv-mode :source "elpaca-menu-lock-file" :recipe
		   (:package "csv-mode" :repo
					 ("https://github.com/emacsmirror/gnu_elpa"
					  . "csv-mode")
					 :branch "externals/csv-mode" :files
					 ("*" (:exclude ".git")) :source
					 "elpaca-menu-lock-file" :protocol https :inherit
					 t :depth treeless :ref
					 "ba5dc934b9dbdc2b57ab1917a669cdfd7d1838d3" :id
					 csv-mode :type git))
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
							 :source "elpaca-menu-lock-file" :protocol
							 https :inherit t :depth treeless :ref
							 "125a718e73f826f461856aabd19bb2de9327531b"
							 :id currency-convert :type git))
 (dash :source "elpaca-menu-lock-file" :recipe
	   (:package "dash" :fetcher github :repo "magnars/dash.el" :files
				 ("dash.el" "dash.texi") :source
				 "elpaca-menu-lock-file" :protocol https :inherit t
				 :depth treeless :ref
				 "fcb5d831fc08a43f984242c7509870f30983c27c" :id dash
				 :type git))
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
					 :source "elpaca-menu-lock-file" :protocol https
					 :inherit t :depth treeless :ref
					 "edb1957d0d6033698c6784686b27508034003fa0" :id
					 deadgrep :type git))
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
					:source "elpaca-menu-lock-file" :protocol https
					:inherit t :depth treeless :ref
					"d2214d34cdeb4483a594dd6973fcd095cef4653f" :id
					devdocs :type git))
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
					:source "elpaca-menu-lock-file" :protocol https
					:inherit t :depth treeless :ref
					"a44db7a69a17a39d5c51bcf22de4decabf0183ad" :id
					diff-hl :type git))
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
					 :source "elpaca-menu-lock-file" :protocol https
					 :inherit t :depth treeless :wait t :ref
					 "fbd5d846611bad828e336b25d2e131d1bc06b83d" :id
					 diminish :type git))
 (dired-hacks-utils :source "elpaca-menu-lock-file" :recipe
					(:package "dired-hacks-utils" :fetcher github
							  :repo "Fuco1/dired-hacks" :files
							  ("dired-hacks-utils.el") :source
							  "elpaca-menu-lock-file" :protocol https
							  :inherit t :depth treeless :ref
							  "de9336f4b47ef901799fe95315fa080fa6d77b48"
							  :id dired-hacks-utils :type git))
 (dired-narrow :source "elpaca-menu-lock-file" :recipe
			   (:package "dired-narrow" :fetcher github :repo
						 "Fuco1/dired-hacks" :files
						 ("dired-narrow.el") :source
						 "elpaca-menu-lock-file" :protocol https
						 :inherit t :depth treeless :ref
						 "de9336f4b47ef901799fe95315fa080fa6d77b48"
						 :id dired-narrow :type git))
 (dired-rainbow :source "elpaca-menu-lock-file" :recipe
				(:package "dired-rainbow" :fetcher github :repo
						  "Fuco1/dired-hacks" :files
						  ("dired-rainbow.el") :source
						  "elpaca-menu-lock-file" :id dired-rainbow
						  :type git :protocol https :inherit t :depth
						  treeless :ref
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
						  :source "elpaca-menu-lock-file" :protocol
						  https :inherit t :depth treeless :ref
						  "3bc8927ed4d14a017eefc75d5af65022343e2ac1"
						  :id dired-sidebar :type git))
 (dired-subtree :source "elpaca-menu-lock-file" :recipe
				(:package "dired-subtree" :fetcher github :repo
						  "Fuco1/dired-hacks" :files
						  ("dired-subtree.el") :source
						  "elpaca-menu-lock-file" :protocol https
						  :inherit t :depth treeless :ref
						  "de9336f4b47ef901799fe95315fa080fa6d77b48"
						  :id dired-subtree :type git))
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
							:source "elpaca-menu-lock-file" :protocol
							https :inherit t :depth treeless :ref
							"8135740bfc6ad96ab82d39d9fe68dbce56180f4c"
							:id dockerfile-mode :type git))
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
					  :source "elpaca-menu-lock-file" :protocol https
					  :inherit t :depth treeless :ref
					  "de7d66c3c864a4722a973ee9bc228a14be49ba0c" :id
					  easy-kill :type git))
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
							 :source "elpaca-menu-lock-file" :protocol
							 https :inherit t :depth treeless :ref
							 "ffc8a332893f26b43eb28ba56a714f875cc14183"
							 :id easy-kill-extras :type git))
 (eat :source "elpaca-menu-lock-file" :recipe
	  (:package "eat" :repo
				("https://codeberg.org/akib/emacs-eat" . "eat") :tar
				"0.9.4" :host nongnu :files ("*" (:exclude ".git"))
				:source "elpaca-menu-lock-file" :id eat :type git
				:protocol https :inherit t :depth treeless :ref
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
				:source "elpaca-menu-lock-file" :id eca :type git
				:protocol https :inherit t :depth treeless :ref
				"ecf9d229e012ceabb871b261c28cc06702d601a7"))
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
						  :source "elpaca-menu-lock-file" :protocol
						  https :inherit t :depth treeless :ref
						  "82a28d8a85277cfe453af464603ea330eae41c05"
						  :id edit-indirect :type git))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source nil :package "elpaca" :id elpaca :repo
			"https://github.com/progfolio/elpaca.git" :ref
			"9c9477d1154978d77b6eab9fcd68475826604188" :depth 1
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
							   :build (:not elpaca-build-docs) :source
							   "Elpaca extensions" :id
							   elpaca-use-package :type git :protocol
							   https :inherit t :depth treeless :ref
							   "9c9477d1154978d77b6eab9fcd68475826604188"))
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
					:source "elpaca-menu-lock-file" :protocol https
					:inherit t :depth treeless :ref
					"38f3bd41096cb270919b06095da0b9ac1add4598" :id
					elquery :type git))
 (emacsql :source "elpaca-menu-lock-file" :recipe
		  (:package "emacsql" :fetcher github :repo "magit/emacsql"
					:files (:defaults "README.md" "sqlite") :source
					"elpaca-menu-lock-file" :protocol https :inherit t
					:depth treeless :ref
					"3e015ab99e061f3967a154683f068ce6553f168a" :id
					emacsql :type git))
 (emms :source "elpaca-menu-lock-file" :recipe
	   (:package "emms" :fetcher git :url
				 "https://git.savannah.gnu.org/git/emms.git" :files
				 (:defaults (:exclude "doc/fdl.texi" "doc/gpl.texi"))
				 :source "GNU ELPA" :protocol https :inherit t :depth
				 treeless :ref
				 "e5f46561c3c140d774354f91d1e95c60039a1934" :id emms
				 :type git))
 (emojify :source "elpaca-menu-lock-file" :recipe
		  (:package "emojify" :fetcher github :repo
					"iqbalansari/emacs-emojify" :files
					(:defaults "data" "images") :source
					"elpaca-menu-lock-file" :protocol https :inherit t
					:depth treeless :ref
					"1b726412f19896abf5e4857d4c32220e33400b55" :id
					emojify :type git))
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
						  :source "elpaca-menu-lock-file" :protocol
						  https :inherit t :depth treeless :ref
						  "04e501e02c475bd9067eebcf8807c951f2316194"
						  :id eshell-toggle :type git))
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
					  :source "elpaca-menu-lock-file" :protocol https
					  :inherit t :depth treeless :ref
					  "1999afaa509204b780db44e99ac9648fe7d92d32" :id
					  eshell-up :type git))
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
					 :source "elpaca-menu-lock-file" :protocol https
					 :inherit t :depth treeless :ref
					 "337cb241e17bd472bd3677ff166a0800f684213c" :id
					 eshell-z :type git))
 (esxml :source "elpaca-menu-lock-file" :recipe
		(:package "esxml" :fetcher github :repo "tali713/esxml" :files
				  ("esxml.el" "esxml-query.el") :source
				  "elpaca-menu-lock-file" :protocol https :inherit t
				  :depth treeless :ref
				  "affada143fed7e2da08f2b3d927a027f26ad4a8f" :id esxml
				  :type git))
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
						  :source "elpaca-menu-lock-file" :protocol
						  https :inherit t :depth treeless :ref
						  "351279272330cae6cecea941b0033a8dd8bcc4e8"
						  :id expand-region :type git))
 (f :source "elpaca-menu-lock-file" :recipe
	(:package "f" :fetcher github :repo "rejeep/f.el" :files
			  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			   "docs/*.texinfo"
			   (:exclude ".dir-locals.el" "test.el" "tests.el"
						 "*-test.el" "*-tests.el" "LICENSE" "README*"
						 "*-pkg.el"))
			  :source "elpaca-menu-lock-file" :protocol https :inherit
			  t :depth treeless :ref
			  "931b6d0667fe03e7bf1c6c282d6d8d7006143c52" :id f :type
			  git))
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
				  :source "elpaca-menu-lock-file" :protocol https
				  :inherit t :depth treeless :ref
				  "a58e29e70948512dbcdbb24745e3fb0a59984925" :id focus
				  :type git))
 (forge :source "elpaca-menu-lock-file" :recipe
		(:package "forge" :fetcher github :repo "magit/forge" :files
				  ("lisp/*.el" "docs/*.texi" ".dir-locals.el") :source
				  "elpaca-menu-lock-file" :protocol https :inherit t
				  :depth treeless :ref
				  "a31859547a1ea5e2acbab67b6b64f90134e2a156" :id forge
				  :type git))
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
					  :source "elpaca-menu-lock-file" :protocol https
					  :inherit t :depth treeless :ref
					  "6258fe6390a7bf264a6f02813502bf83a645d872" :id
					  form-feed :type git))
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
					  :type git :protocol https :inherit t :depth
					  treeless :ref
					  "bed8e9c356c889cd98dd7a4a63c69d6c4960cf82"))
 (ghub :source "elpaca-menu-lock-file" :recipe
	   (:package "ghub" :fetcher github :repo "magit/ghub" :files
				 ("lisp/*.el" "docs/*.texi" ".dir-locals.el") :source
				 "elpaca-menu-lock-file" :protocol https :inherit t
				 :depth treeless :ref
				 "97a07691efad6fc16bc000a35be80d4f8dae251a" :id ghub
				 :type git))
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
							:source "elpaca-menu-lock-file" :protocol
							https :inherit t :depth treeless :ref
							"d1346a76122595aeeb7ebb292765841c6cfd417b"
							:id git-timemachine :type git))
 (gntp :source "elpaca-menu-lock-file" :recipe
	   (:package "gntp" :repo "tekai/gntp.el" :fetcher github :files
				 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
				  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
				  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
				  "docs/*.texinfo"
				  (:exclude ".dir-locals.el" "test.el" "tests.el"
							"*-test.el" "*-tests.el" "LICENSE"
							"README*" "*-pkg.el"))
				 :source "elpaca-menu-lock-file" :protocol https
				 :inherit t :depth treeless :ref
				 "767571135e2c0985944017dc59b0be79af222ef5" :id gntp
				 :type git))
 (go-mode :source "elpaca-menu-lock-file" :recipe
		  (:package "go-mode" :repo "dominikh/go-mode.el" :fetcher
					github :files ("go-mode.el") :source
					"elpaca-menu-lock-file" :protocol https :inherit t
					:depth treeless :ref
					"0ed3c5227e7f622589f1411b4939c3ee34711ebd" :id
					go-mode :type git))
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
					:source "elpaca-menu-lock-file" :protocol https
					:inherit t :depth treeless :ref
					"6f87a700137c838568966bc8099dc15786897c32" :id
					goggles :type git))
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
				  :source "elpaca-menu-lock-file" :protocol https
				  :inherit t :depth treeless :ref
				  "e49685d95b8f729cc9c585aa07c3c237a5f81790" :id gptel
				  :type git))
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
							  :source "elpaca-menu-lock-file"
							  :protocol https :inherit t :depth
							  treeless :ref
							  "516c151b845a3eb2da73eb4ee648ad99172087ac"
							  :id graphviz-dot-mode :type git))
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
					 :source "elpaca-menu-lock-file" :protocol https
					 :inherit t :depth treeless :ref
					 "1da895ed75d28d9f87cbf9b74f075d90ba31c0ed" :id
					 hcl-mode :type git))
 (highlight-indent-guides :source "elpaca-menu-lock-file" :recipe
						  (:package "highlight-indent-guides" :fetcher
									github :repo
									"DarthFennec/highlight-indent-guides"
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
									:protocol https :inherit t :depth
									treeless :ref
									"d05c63f395329c6e331e0b44c7db708005b3316f"
									:id highlight-indent-guides :type
									git))
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
							  :source "elpaca-menu-lock-file"
							  :protocol https :inherit t :depth
							  treeless :ref
							  "8b4744c7f46c72b1d3d599d4fb75ef8183dee307"
							  :id highlight-numbers :type git))
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
					:source "elpaca-menu-lock-file" :protocol https
					:inherit t :depth treeless :ref
					"2e9504511aa393686f44a36716c1d2ebdc5def1f" :id
					hl-todo :type git))
 (howm :source "elpaca-menu-lock-file" :recipe
	   (:package "howm" :fetcher github :repo "kaorahi/howm" :files
				 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
				  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
				  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
				  "docs/*.texinfo"
				  (:exclude ".dir-locals.el" "test.el" "tests.el"
							"*-test.el" "*-tests.el" "LICENSE"
							"README*" "*-pkg.el"))
				 :source "elpaca-menu-lock-file" :protocol ssh
				 :inherit t :depth treeless :ref
				 "d1431c9e678de6e0c179b2a2bf405e8804472fee" :id howm
				 :remotes (("fork" :repo "nicolaisingh/howm")) :type
				 git))
 (ht :source "elpaca-menu-lock-file" :recipe
	 (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
			   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
				"doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
				"lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
				"docs/*.texinfo"
				(:exclude ".dir-locals.el" "test.el" "tests.el"
						  "*-test.el" "*-tests.el" "LICENSE" "README*"
						  "*-pkg.el"))
			   :source "elpaca-menu-lock-file" :protocol https
			   :inherit t :depth treeless :ref
			   "1c49aad1c820c86f7ee35bf9fff8429502f60fef" :id ht :type
			   git))
 (htmlize :source "elpaca-menu-lock-file" :recipe
		  (:package "htmlize" :fetcher github :repo
					"hniksic/emacs-htmlize" :files
					("*.el" "*.el.in" "dir" "*.info" "*.texi"
					 "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
					 "doc/*.texinfo" "lisp/*.el" "docs/dir"
					 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
					 (:exclude ".dir-locals.el" "test.el" "tests.el"
							   "*-test.el" "*-tests.el" "LICENSE"
							   "README*" "*-pkg.el"))
					:source "elpaca-menu-lock-file" :protocol https
					:inherit t :depth treeless :ref
					"a24ed5a939a32c625b034b0b6d7f46340c100394" :id
					htmlize :type git))
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
						   :source "elpaca-menu-lock-file" :protocol
						   https :inherit t :depth treeless :ref
						   "8ab49b066bc23de731a29ef07bbafa29999e1852"
						   :id json-navigator :type git))
 (know-your-http-well :source "elpaca-menu-lock-file" :recipe
					  (:package "know-your-http-well" :fetcher github
								:repo "for-GET/know-your-http-well"
								:files ("emacs/*.el") :source
								"elpaca-menu-lock-file" :protocol
								https :inherit t :depth treeless :ref
								"c916e825b0c75c2f2b6ff5ba79c981cff7de5797"
								:id know-your-http-well :type git))
 (llama :source "elpaca-menu-lock-file" :recipe
		(:package "llama" :fetcher github :repo "tarsius/llama" :files
				  ("llama.el" ".dir-locals.el") :source
				  "elpaca-menu-lock-file" :protocol https :inherit t
				  :depth treeless :ref
				  "6a67e4253cc02aa9ce85ef96290c95198b65d913" :id llama
				  :type git))
 (llm-tool-collection :source "elpaca-menu-lock-file" :recipe
					  (:source "elpaca-menu-lock-file" :protocol https
							   :inherit t :depth treeless :repo
							   "~/.emacs.d/packages/llm-tool-collection"
							   :package "llm-tool-collection" :ref
							   "a383ccf3df6c86684da77fb61ea4ebe67a21eedb"
							   :id llm-tool-collection :type git))
 (log4e :source "elpaca-menu-lock-file" :recipe
		(:package "log4e" :repo "aki2o/log4e" :fetcher github :files
				  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
				   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
				   "doc/*.texinfo" "lisp/*.el" "docs/dir"
				   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
				   (:exclude ".dir-locals.el" "test.el" "tests.el"
							 "*-test.el" "*-tests.el" "LICENSE"
							 "README*" "*-pkg.el"))
				  :source "elpaca-menu-lock-file" :protocol https
				  :inherit t :depth treeless :ref
				  "6d71462df9bf595d3861bfb328377346aceed422" :id log4e
				  :type git))
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
						:source "elpaca-menu-lock-file" :protocol
						https :inherit t :depth treeless :ref
						"4e87a899868e908a7a9e1812831d76c8d072f885" :id
						lorem-ipsum :type git))
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
					  :source "elpaca-menu-lock-file" :protocol https
					  :inherit t :depth treeless :ref
					  "d0928626b4711dcf9f8f90439d23701118724199" :id
					  macrostep :type git))
 (magit :source "elpaca-menu-lock-file" :recipe
		(:package "magit" :fetcher github :repo "magit/magit" :files
				  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi"
				   "docs/AUTHORS.md" "LICENSE" ".dir-locals.el"
				   (:exclude "lisp/magit-section.el"))
				  :source "elpaca-menu-lock-file" :protocol https
				  :inherit t :depth treeless :ref
				  "31afaddc07b2a9808178d4c59d8c896146d4dfad" :id magit
				  :type git))
 (magit-section :source "elpaca-menu-lock-file" :recipe
				(:package "magit-section" :fetcher github :repo
						  "magit/magit" :files
						  ("lisp/magit-section.el"
						   "docs/magit-section.texi"
						   "magit-section-pkg.el")
						  :source "elpaca-menu-lock-file" :protocol
						  https :inherit t :depth treeless :ref
						  "31afaddc07b2a9808178d4c59d8c896146d4dfad"
						  :id magit-section :type git))
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
						:source "elpaca-menu-lock-file" :protocol
						https :inherit t :depth treeless :ref
						"bd27c57eada0fda1cc0a813db04731a9bcc51b7b" :id
						magit-todos :type git))
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
					   :source "elpaca-menu-lock-file" :protocol https
					   :inherit t :depth treeless :ref
					   "0e7097051cbcedcc4da9b633406291d4052ec0e4" :id
					   marginalia :type git))
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
						  :source "elpaca-menu-lock-file" :protocol
						  https :inherit t :depth treeless :ref
						  "7c20685df615469132763add897c1ca37cbe07e4"
						  :id markdown-mode :type git))
 (mathjax :source "elpaca-menu-lock-file" :recipe
		  (:package "mathjax" :repo
					("https://github.com/astoff/mathjax.el"
					 . "mathjax")
					:files ("*" (:exclude ".git")) :source
					"elpaca-menu-lock-file" :protocol https :inherit t
					:depth treeless :ref
					"db669451bbee7d2ea9872c28661c4679391b9644" :id
					mathjax :type git))
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
				:source "elpaca-menu-lock-file" :protocol https
				:inherit t :depth treeless :ref
				"963b4af6ce743fbb6224f61bb61f05de1c37f511" :id mcp
				:type git))
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
						 :source "elpaca-menu-lock-file" :protocol
						 https :inherit t :depth treeless :ref
						 "e74d4da7612c7a88e07f9dd3369e3b9fd36f396c"
						 :id mermaid-mode :type git))
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
						multi-vterm :type git :protocol https :inherit
						t :depth treeless :ref
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
							 :source "elpaca-menu-lock-file" :protocol
							 https :inherit t :depth treeless :ref
							 "89f1a8df9b1fc721b1672b4c7b6d3ab451e7e3ef"
							 :id multiple-cursors :type git))
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
						:source "elpaca-menu-lock-file" :protocol
						https :inherit t :depth treeless :ref
						"62ce3a2dc39529c5db3516427e84b2c96b8efcfd" :id
						nix-ts-mode :type git))
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
				:source "elpaca-menu-lock-file" :protocol https
				:inherit t :depth treeless :ref
				"b37d9380752e541db3f4b947c219ca54d50ca273" :id nov
				:type git))
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
					   :source "elpaca-menu-lock-file" :protocol https
					   :inherit t :depth treeless :ref
					   "4f814ad5e1b96764af52d53799288ff3acc94b47" :id
					   ob-mermaid :type git))
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
							  operate-on-number :type git :protocol
							  https :inherit t :depth treeless :ref
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
					  :source "elpaca-menu-lock-file" :protocol https
					  :inherit t :depth treeless :ref
					  "254f2412489bbbf62700f9d3d5f18e537841dcc3" :id
					  orderless :type git))
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
								 :source "elpaca-menu-lock-file"
								 :protocol https :inherit t :depth
								 treeless :ref
								 "bcab006ec42d7e2c92875c7170df193de2ee55f5"
								 :id org-bookmark-heading :type git))
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
						:source "elpaca-menu-lock-file" :protocol
						https :inherit t :depth treeless :ref
						"4ec04e1b77dea76d7c30066ccf3200d2e0b7bee9" :id
						org-present :type git))
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
							  :source "elpaca-menu-lock-file"
							  :protocol https :inherit t :depth
							  treeless :ref
							  "697875935b04b25c8229b9155a1ea0cab3ebe629"
							  :id org-sticky-header :type git))
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
						  :source "elpaca-menu-lock-file" :protocol
						  https :inherit t :depth treeless :ref
						  "7a6498f442fc7f29504745649948635c7165d847"
						  :id org-web-tools :type git))
 (orgalist :source "elpaca-menu-lock-file" :recipe
		   (:package "orgalist" :repo
					 ("https://github.com/emacsmirror/gnu_elpa"
					  . "orgalist")
					 :branch "externals/orgalist" :files
					 ("*" (:exclude ".git")) :source
					 "elpaca-menu-lock-file" :protocol https :inherit
					 t :depth treeless :ref
					 "1f540f645eeb338019750d4dca3c056a1f5eca6d" :id
					 orgalist :type git))
 (osm :source "elpaca-menu-lock-file" :recipe
	  (:package "osm" :repo "minad/osm" :fetcher github :files
				("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
				 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
				 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
				 "docs/*.texinfo"
				 (:exclude ".dir-locals.el" "test.el" "tests.el"
						   "*-test.el" "*-tests.el" "LICENSE"
						   "README*" "*-pkg.el"))
				:source "elpaca-menu-lock-file" :protocol https
				:inherit t :depth treeless :ref
				"0c976bc0cd1b61f2e7761703f30d6731bb2984cd" :id osm
				:type git))
 (ox-asciidoc :source "elpaca-menu-lock-file" :recipe
			  (:package "ox-asciidoc" :fetcher github :repo
						"yashi/org-asciidoc" :files ("ox-asciidoc.el")
						:source "elpaca-menu-lock-file" :protocol
						https :inherit t :depth treeless :ref
						"a8d49c44cc9aa8a3f384155f0ae052dbf36df00c" :id
						ox-asciidoc :type git))
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
				   :source "elpaca-menu-lock-file" :protocol https
				   :inherit t :depth treeless :ref
				   "4f774f13d34b3db9ea4ddb0b1edc070b1526ccbb" :id
				   ox-gfm :type git))
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
					:source "elpaca-menu-lock-file" :protocol https
					:inherit t :depth treeless :ref
					"8748a908ee366e52539b19b863935d2ef7ebd0bf" :id
					ox-jira :type git))
 (package-lint :source "elpaca-menu-lock-file" :recipe
			   (:package "package-lint" :fetcher github :repo
						 "purcell/package-lint" :files
						 (:defaults "data" (:exclude "*flymake.el"))
						 :source "elpaca-menu-lock-file" :protocol
						 https :inherit t :depth treeless :ref
						 "29ccfea319ee6bac4b80045aa473b3aa2b77d074"
						 :id package-lint :type git))
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
						:source "elpaca-menu-lock-file" :protocol
						https :inherit t :depth treeless :ref
						"fbd49857ab2e4cd0c5611c0cc83f93711657b298" :id
						parent-mode :type git))
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
				 :source "elpaca-menu-lock-file" :protocol https
				 :inherit t :depth treeless :ref
				 "896696999dde9b6ac950ab485cb09f8701ad7626" :id pass
				 :type git))
 (password-store :source "elpaca-menu-lock-file" :recipe
				 (:package "password-store" :fetcher github :repo
						   "zx2c4/password-store" :files
						   ("contrib/emacs/*.el") :source
						   "elpaca-menu-lock-file" :protocol https
						   :inherit t :depth treeless :ref
						   "b5e965a838bb68c1227caa2cdd874ba496f10149"
						   :id password-store :type git))
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
							   :source "elpaca-menu-lock-file"
							   :protocol ssh :inherit t :depth
							   treeless :ref
							   "12e0e6ba0d4bef485153f05d2916298cd1a4ef7d"
							   :id password-store-otp :remotes
							   (("fork" :repo
								 "nicolaisingh/password-store-otp.el"))
							   :type git))
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
					:source "elpaca-menu-lock-file" :protocol https
					:inherit t :depth treeless :ref
					"b4d846d80dddb313042131cf2b8fbf647567e000" :id
					pcre2el :type git))
 (pdf-tools :source "elpaca-menu-lock-file" :recipe
			(:package "pdf-tools" :fetcher github :repo
					  "vedang/pdf-tools" :files
					  (:defaults "README" ("build" "Makefile")
								 ("build" "server"))
					  :source "elpaca-menu-lock-file" :protocol https
					  :inherit t :depth treeless :ref
					  "30b50544e55b8dbf683c2d932d5c33ac73323a16" :id
					  pdf-tools :type git))
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
						  :source "elpaca-menu-lock-file" :protocol
						  https :inherit t :depth treeless :ref
						  "dc0f497a67ee9b3d2e437e16023011780d0e8b05"
						  :id plantuml-mode :type git))
 (plz :source "elpaca-menu-lock-file" :recipe
	  (:package "plz" :repo
				("https://github.com/alphapapa/plz.el.git" . "plz")
				:files ("*" (:exclude ".git" "LICENSE")) :source
				"elpaca-menu-lock-file" :protocol https :inherit t
				:depth treeless :ref
				"e2d07838e3b64ee5ebe59d4c3c9011adefb7b58e" :id plz
				:type git))
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
						:source "elpaca-menu-lock-file" :protocol
						https :inherit t :depth treeless :ref
						"e9b73e81d3e1642aec682195f127a42dfb0b5774" :id
						prettier-js :type git))
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
				  :source "elpaca-menu-lock-file" :protocol https
				  :inherit t :depth treeless :ref
				  "2fa8eb5a9ca62a548d33befef4517e5d0266eb28" :id prism
				  :type git))
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
					:source "elpaca-menu-lock-file" :protocol https
					:inherit t :depth treeless :ref
					"336450ba90864dec62c8c883554d34aa18132165" :id
					prodigy :type git))
 (protobuf-mode :source "elpaca-menu-lock-file" :recipe
				(:package "protobuf-mode" :fetcher github :repo
						  "protocolbuffers/protobuf" :files
						  ("editors/protobuf-mode.el") :source
						  "elpaca-menu-lock-file" :id protobuf-mode
						  :type git :protocol https :inherit t :depth
						  treeless :ref
						  "78e191081bca2809e5005f3f60c2a72bb3f8aa93"))
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
				   :source "elpaca-menu-lock-file" :protocol https
				   :inherit t :depth treeless :ref
				   "8692f965bf4ddf3d755cf1fbf77a7a768e22460e" :id
				   pytest :type git))
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
				   :source "elpaca-menu-lock-file" :protocol https
				   :inherit t :depth treeless :ref
				   "31ea715f2164dd611e7fc77b26390ef3ca93509b" :id
				   pyvenv :type git))
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
					 :source "elpaca-menu-lock-file" :protocol https
					 :inherit t :depth treeless :ref
					 "30d42b3401841ebbf2036840a2189bf62c18f507" :id
					 qrencode :type git))
 (rainbow-mode :source "elpaca-menu-lock-file" :recipe
			   (:package "rainbow-mode" :repo
						 ("https://github.com/emacsmirror/gnu_elpa"
						  . "rainbow-mode")
						 :branch "externals/rainbow-mode" :files
						 ("*" (:exclude ".git")) :source
						 "elpaca-menu-lock-file" :protocol https
						 :inherit t :depth treeless :ref
						 "f7db3b5919f70420a91eb199f8663468de3033f3"
						 :id rainbow-mode :type git))
 (request :source "elpaca-menu-lock-file"
   :recipe
   (:package "request" :repo "tkf/emacs-request" :fetcher github
			 :files ("request.el") :source "elpaca-menu-lock-file"
			 :protocol https :inherit t :depth treeless :ref
			 "c22e3c23a6dd90f64be536e176ea0ed6113a5ba6" :id request
			 :type git))
 (restclient :source "elpaca-menu-lock-file" :recipe
			 (:package "restclient" :repo "pashky/restclient.el"
					   :fetcher github :files ("restclient.el")
					   :source "elpaca-menu-lock-file" :protocol https
					   :inherit t :depth treeless :ref
					   "e2a2b13482d72634f8e49864cd9e5c907a5fe137" :id
					   restclient :type git))
 (s :source "elpaca-menu-lock-file" :recipe
	(:package "s" :fetcher github :repo "magnars/s.el" :files
			  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			   "docs/*.texinfo"
			   (:exclude ".dir-locals.el" "test.el" "tests.el"
						 "*-test.el" "*-tests.el" "LICENSE" "README*"
						 "*-pkg.el"))
			  :source "elpaca-menu-lock-file" :protocol https :inherit
			  t :depth treeless :ref
			  "dda84d38fffdaf0c9b12837b504b402af910d01d" :id s :type
			  git))
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
							   :source "elpaca-menu-lock-file"
							   :protocol ssh :inherit t :depth
							   treeless :ref
							   "dc1e0b28a5ed8319a0b6725abaffba7c2fa8c730"
							   :id saveplace-pdf-view :type git))
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
					 :source "elpaca-menu-lock-file" :protocol https
					 :inherit t :depth treeless :ref
					 "1ca6e12f456caa1dc97c3d68597598662eb5de9a" :id
					 selected :type git))
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
						shell-maker :type git :protocol https :inherit
						t :depth treeless :ref
						"6377cbdb49248d670170f1c8dbe045648063583e"))
 (slime :source "elpaca-menu-lock-file" :recipe
		(:package "slime" :fetcher github :repo "slime/slime" :files
				  ("*.el" ("lib" "lib/hyperspec.el") "swank" "*.lisp"
				   "*.asd" "doc/slime.texi" "doc/slime.info" "doc/dir"
				   "ChangeLog" ("contrib" "contrib/*")
				   (:exclude "contrib/test" "contrib/Makefile"))
				  :source "elpaca-menu-lock-file" :protocol https
				  :inherit t :depth treeless :ref
				  "86b68025a9dcd6212ca6e27b228b42950c479ef3" :id slime
				  :type git))
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
						:source "elpaca-menu-lock-file" :protocol
						https :inherit t :depth treeless :ref
						"603325ab3d1186fb10da5c2a7ec1afb88018d792" :id
						smartparens :type git))
 (spacious-padding :source "elpaca-menu-lock-file" :recipe
				   (:package "spacious-padding" :repo
							 ("https://github.com/protesilaos/spacious-padding"
							  . "spacious-padding")
							 :tar "0.8.0" :host gnu :files
							 ("*"
							  (:exclude ".git" "COPYING"
										"doclicense.texi"))
							 :source "elpaca-menu-lock-file" :id
							 spacious-padding :type git :protocol
							 https :inherit t :depth treeless :ref
							 "a9cddfb2e2d212d7723104fb049e34f30240f063"))
 (spinner :source "elpaca-menu-lock-file" :recipe
		  (:package "spinner" :repo
					("https://github.com/Malabarba/spinner.el"
					 . "spinner")
					:files ("*" (:exclude ".git")) :source
					"elpaca-menu-lock-file" :protocol https :inherit t
					:depth treeless :ref
					"d4647ae87fb0cd24bc9081a3d287c860ff061c21" :id
					spinner :type git))
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
					:source "elpaca-menu-lock-file" :protocol https
					:inherit t :depth treeless :ref
					"fcd37147121fabdf003a70279cf86fbe08cfac6f" :id
					tablist :type git))
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
						   :source "elpaca-menu-lock-file" :protocol
						   https :inherit t :depth treeless :ref
						   "01635df3625c0cec2bb4613a6f920b8569d41009"
						   :id terraform-mode :type git))
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
					  :source "elpaca-menu-lock-file" :protocol https
					  :inherit t :depth treeless :ref
					  "13daf473d5080b91bc37f40e0f3b566738707914" :id
					  transient :type git))
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
					  :source "elpaca-menu-lock-file" :protocol https
					  :inherit t :depth treeless :ref
					  "b06078826d5875d74b0e7b7ac47b0d0917610534" :id
					  tree-mode :type git))
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
				   :source "elpaca-menu-lock-file" :protocol https
				   :inherit t :depth treeless :ref
				   "651e2634f01f346da9ec8a64613c51f54b444bc3" :id
				   treepy :type git))
 (treesit-fold :source "elpaca-menu-lock-file" :recipe
			   (:package "treesit-fold" :repo
						 ("https://github.com/emacs-tree-sitter/treesit-fold"
						  . "treesit-fold")
						 :files ("*" (:exclude ".git")) :source
						 "elpaca-menu-lock-file" :protocol https
						 :inherit t :depth treeless :ref
						 "5b76926c14436b252150f88a8cd7e125ab295bcb"
						 :id treesit-fold :type git))
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
				   :source "elpaca-menu-lock-file" :protocol https
				   :inherit t :depth treeless :ref
				   "4a15511876983eeaa75e57fcab8d4d51fe9b3840" :id
				   unfill :type git))
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
					:source "elpaca-menu-lock-file" :protocol https
					:inherit t :depth treeless :ref
					"40b10f363e1f0bb31418fad6ab11ed2b0d57278a" :id
					uniline :type git))
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
					:source "elpaca-menu-lock-file" :protocol https
					:inherit t :depth treeless :ref
					"cebbe09d27c63abe61fe8c2e2248587d90265b59" :id
					uuidgen :type git))
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
							   :source "elpaca-menu-lock-file"
							   :protocol https :inherit t :depth
							   treeless :ref
							   "e391b52922086ac38397a3325933900b6d90f9f0"
							   :id visual-fill-column :type git))
 (vterm :source "elpaca-menu-lock-file" :recipe
		(:package "vterm" :fetcher github :repo
				  "akermu/emacs-libvterm" :files
				  ("CMakeLists.txt" "elisp.c" "elisp.h"
				   "emacs-module.h" "etc" "utf8.c" "utf8.h" "vterm.el"
				   "vterm-module.c" "vterm-module.h")
				  :source "elpaca-menu-lock-file" :id vterm :type git
				  :protocol https :inherit t :depth treeless :ref
				  "54c29d14bca05bdd8ae60cda01715d727831e3f9"))
 (vundo :source "elpaca-menu-lock-file" :recipe
		(:package "vundo" :repo
				  ("https://github.com/casouri/vundo" . "vundo")
				  :files ("*" (:exclude ".git" "test")) :source
				  "elpaca-menu-lock-file" :protocol https :inherit t
				  :depth treeless :ref
				  "30f85b4ae1f2a7189d44bb738b49559928d046cb" :id vundo
				  :type git))
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
			 :source "elpaca-menu-lock-file" :protocol https :inherit
			 t :depth treeless :ref
			 "f32cd7b09d518b629bfaa3eeb92b539891c6b9bc" :id
			 with-editor :type git))
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
						   :source "elpaca-menu-lock-file" :protocol
						   https :inherit t :depth treeless :ref
						   "cca2b4b3cfcfea1919e1870519d79ed1a69aa5e2"
						   :id writeroom-mode :type git))
 (yaml :source "elpaca-menu-lock-file" :recipe
	   (:package "yaml" :repo "zkry/yaml.el" :fetcher github :files
				 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
				  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
				  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
				  "docs/*.texinfo"
				  (:exclude ".dir-locals.el" "test.el" "tests.el"
							"*-test.el" "*-tests.el" "LICENSE"
							"README*" "*-pkg.el"))
				 :source "elpaca-menu-lock-file" :protocol https
				 :inherit t :depth treeless :ref
				 "f99ef76c80e6fc3fcf650c4fe34e10726594a4c4" :id yaml
				 :type git))
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
					 :source "elpaca-menu-lock-file" :protocol https
					 :inherit t :depth treeless :ref
					 "8dce99ba3e5fcb5e2c3de360c4ee658055051f33" :id
					 yaml-pro :type git))
 (yasnippet :source "elpaca-menu-lock-file" :recipe
			(:package "yasnippet" :repo "joaotavora/yasnippet"
					  :fetcher github :files
					  ("yasnippet.el" "snippets") :source
					  "elpaca-menu-lock-file" :protocol https :inherit
					  t :depth treeless :ref
					  "c1e6ff23e9af16b856c88dfaab9d3ad7b746ad37" :id
					  yasnippet :type git))
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
				  :source "elpaca-menu-lock-file" :protocol https
				  :inherit t :depth treeless :ref
				  "9905f1be006fe02417fc6598be4990746053bbec" :id ztree
				  :type git)))
