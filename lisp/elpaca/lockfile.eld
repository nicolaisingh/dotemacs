((aggressive-indent :source "elpaca-menu-lock-file" :recipe
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
			      "a437a45868f94b77362c6b913c5ee8e67b273c42"))
 (aider :source "elpaca-menu-lock-file" :recipe
	(:package "aider" :fetcher github :repo "tninja/aider.el"
		  :files (:defaults "snippets") :source
		  "elpaca-menu-lock-file" :protocol https :inherit t
		  :depth treeless :ref
		  "e11be58103c03b49f6dce4d89a4068017fdfa95c"))
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
		  "7774b5fd2feb98d4910ff06435d08c19fba93e26"))
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
		  "bb3f31966ed65a76abe6fa4f80a960a2917f554e"))
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
		    :source "elpaca-menu-lock-file" :protocol https
		    :inherit t :depth treeless :ref
		    "196cc080f2aaec33caa1b9298e3a9f3dc52c26c5"))
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
			     "03cc18c08a549568edb6bc710c307c19bc507ef3"))
 (calibre :source "elpaca-menu-lock-file" :recipe
	  (:package "calibre" :repo
		    ("https://git.disroot.org/kjartanoli/calibre.el"
		     . "calibre")
		    :files ("*" (:exclude ".git")) :source
		    "elpaca-menu-lock-file" :protocol https :inherit t
		    :depth treeless :ref
		    "855a9cb092eecf1e5c17ea56c440a0200f95eb4c"))
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
		 "c9191ee9e13e86a7b40c3d25c8bf7907c085a1cf"))
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
				 "67ef719e685407dbc455c7430765e4e685fd95a9"))
 (chatgpt-shell :source "elpaca-menu-lock-file" :recipe
		(:package "chatgpt-shell" :fetcher github :repo
			  "xenodium/chatgpt-shell" :files
			  ("*.el"
			   (:exclude "test_chatgpt-shell.el"
				     "shell-maker.el"
				     "ob-chatgpt-shell.el"
				     "dall-e-shell.el"
				     "ob-dall-e-shell.el"))
			  :source "elpaca-menu-lock-file" :protocol
			  https :inherit t :depth treeless :ref
			  "d5a6c91d80de7bbf9958fc6782bcdd0a852d8963"))
 (chess :source "elpaca-menu-lock-file" :recipe
	(:package "chess" :repo
		  ("https://github.com/emacsmirror/gnu_elpa" . "chess")
		  :branch "externals/chess" :files
		  ("*" (:exclude ".git")) :source
		  "elpaca-menu-lock-file" :protocol https :inherit t
		  :depth treeless :ref
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
		    :source "elpaca-menu-lock-file" :protocol https
		    :inherit t :depth treeless :ref
		    "5ea0bf7c3881ea905e280446342539b242401979"))
 (clojure-mode :source "elpaca-menu-lock-file" :recipe
	       (:package "clojure-mode" :repo
			 "clojure-emacs/clojure-mode" :fetcher github
			 :files ("clojure-mode.el") :source
			 "elpaca-menu-lock-file" :protocol https
			 :inherit t :depth treeless :ref
			 "28dc02114ae70db6bb68d537ea77985f272120bc"))
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
		   "05a2b048fd4e5c90aa971479cb9e71cf9aeba2bf"))
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
			     "af600e6b4129c8115f464af576505ea8e789db27"))
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
		    "b6fd7feda74267a53f9f1cf1bd122e519a308132"))
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
			"4532b8d215d16b0159691ce4dee693e72d71e0ff"))
 (consult-org-roam :source "elpaca-menu-lock-file" :recipe
		   (:package "consult-org-roam" :repo
			     "jgru/consult-org-roam" :fetcher github
			     :files
			     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			      "*.texinfo" "doc/dir" "doc/*.info"
			      "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			      "docs/dir" "docs/*.info" "docs/*.texi"
			      "docs/*.texinfo"
			      (:exclude ".dir-locals.el" "test.el"
					"tests.el" "*-test.el"
					"*-tests.el" "LICENSE"
					"README*" "*-pkg.el"))
			     :source "MELPA" :protocol https :inherit
			     t :depth treeless :ref
			     "a6dec09dcd06a3014409044399c4f8860ca45ef1"))
 (copilot :source "elpaca-menu-lock-file" :recipe
	  (:package "copilot" :fetcher github :repo
		    "copilot-emacs/copilot.el" :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		     "doc/*.texinfo" "lisp/*.el" "docs/dir"
		     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el"
			       "*-test.el" "*-tests.el" "LICENSE"
			       "README*" "*-pkg.el"))
		    :source "MELPA" :protocol https :inherit t :depth
		    treeless :ref
		    "fe3f51b636dea1c9ac55a0d5dc5d7df02dcbaa48"))
 (corfu :source "elpaca-menu-lock-file" :recipe
	(:package "corfu" :repo "minad/corfu" :files
		  (:defaults "extensions/corfu-*.el") :fetcher github
		  :source "elpaca-menu-lock-file" :protocol https
		  :inherit t :depth treeless :ref
		  "e032f32c7ff24f5aaa44946551920aa94cd2a7f9"))
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
		"7a3599e42d4fe943b912701e04beffcf2ec812d2"))
 (csv-mode :source "elpaca-menu-lock-file" :recipe
	   (:package "csv-mode" :repo
		     ("https://github.com/emacsmirror/gnu_elpa"
		      . "csv-mode")
		     :branch "externals/csv-mode" :files
		     ("*" (:exclude ".git")) :source
		     "elpaca-menu-lock-file" :protocol https :inherit
		     t :depth treeless :ref
		     "ba5dc934b9dbdc2b57ab1917a669cdfd7d1838d3"))
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
			     "125a718e73f826f461856aabd19bb2de9327531b"))
 (dash :source "elpaca-menu-lock-file" :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
		 ("dash.el" "dash.texi") :source
		 "elpaca-menu-lock-file" :protocol https :inherit t
		 :depth treeless :ref
		 "fcb5d831fc08a43f984242c7509870f30983c27c"))
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
		     "edb1957d0d6033698c6784686b27508034003fa0"))
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
		    "d2214d34cdeb4483a594dd6973fcd095cef4653f"))
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
		    "9b032018fda8eb6c241bba1ec0e5c354ad523b2c"))
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
		     "fbd5d846611bad828e336b25d2e131d1bc06b83d"))
 (dired-hacks-utils :source "elpaca-menu-lock-file" :recipe
		    (:package "dired-hacks-utils" :fetcher github
			      :repo "Fuco1/dired-hacks" :files
			      ("dired-hacks-utils.el") :source
			      "elpaca-menu-lock-file" :protocol https
			      :inherit t :depth treeless :ref
			      "de9336f4b47ef901799fe95315fa080fa6d77b48"))
 (dired-narrow :source "elpaca-menu-lock-file" :recipe
	       (:package "dired-narrow" :fetcher github :repo
			 "Fuco1/dired-hacks" :files
			 ("dired-narrow.el") :source
			 "elpaca-menu-lock-file" :protocol https
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
			  :source "elpaca-menu-lock-file" :protocol
			  https :inherit t :depth treeless :ref
			  "3bc8927ed4d14a017eefc75d5af65022343e2ac1"))
 (dired-subtree :source "elpaca-menu-lock-file" :recipe
		(:package "dired-subtree" :fetcher github :repo
			  "Fuco1/dired-hacks" :files
			  ("dired-subtree.el") :source
			  "elpaca-menu-lock-file" :protocol https
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
			    :source "elpaca-menu-lock-file" :protocol
			    https :inherit t :depth treeless :ref
			    "8135740bfc6ad96ab82d39d9fe68dbce56180f4c"))
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
		      "de7d66c3c864a4722a973ee9bc228a14be49ba0c"))
 (eat :source "elpaca-menu-lock-file" :recipe
      (:package "eat" :repo
		("https://codeberg.org/akib/emacs-eat" . "eat") :files
		("*" (:exclude ".git")) :source
		"elpaca-menu-lock-file" :protocol https :inherit t
		:depth treeless :ref
		"c8d54d649872bfe7b2b9f49ae5c2addbf12d3b99"))
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
			  "82a28d8a85277cfe453af464603ea330eae41c05"))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source nil :protocol https :inherit ignore :depth 1 :repo
	    "https://github.com/progfolio/elpaca.git" :ref
	    "029d4b477b13fe675b0cf41099e44b177e5760ff" :files
	    (:defaults "elpaca-test.el" (:exclude "extensions"))
	    :build (:not elpaca--activate-package) :package "elpaca"))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
		     (:package "elpaca-use-package" :wait t :repo
			       "https://github.com/progfolio/elpaca.git"
			       :files
			       ("extensions/elpaca-use-package.el")
			       :main
			       "extensions/elpaca-use-package.el"
			       :build (:not elpaca--compile-info)
			       :source "Elpaca extensions" :protocol
			       https :inherit t :depth treeless :ref
			       "029d4b477b13fe675b0cf41099e44b177e5760ff"))
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
		    "38f3bd41096cb270919b06095da0b9ac1add4598"))
 (emacsql :source "elpaca-menu-lock-file" :recipe
	  (:package "emacsql" :fetcher github :repo "magit/emacsql"
		    :files (:defaults "README.md" "sqlite") :source
		    "elpaca-menu-lock-file" :protocol https :inherit t
		    :depth treeless :ref
		    "ced062890061b6e4fbe4d00c0617f7ff84fff25c"))
 (emms :source "elpaca-menu-lock-file" :recipe
       (:package "emms" :fetcher git :url
		 "https://git.savannah.gnu.org/git/emms.git" :files
		 (:defaults (:exclude "doc/fdl.texi" "doc/gpl.texi"))
		 :source "GNU ELPA" :protocol https :inherit t :depth
		 treeless :ref
		 "e5f46561c3c140d774354f91d1e95c60039a1934"))
 (emojify :source "elpaca-menu-lock-file" :recipe
	  (:package "emojify" :fetcher github :repo
		    "iqbalansari/emacs-emojify" :files
		    (:defaults "data" "images") :source "MELPA"
		    :protocol https :inherit t :depth treeless :ref
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
			  :source "elpaca-menu-lock-file" :protocol
			  https :inherit t :depth treeless :ref
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
		      :source "elpaca-menu-lock-file" :protocol https
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
		     :source "elpaca-menu-lock-file" :protocol https
		     :inherit t :depth treeless :ref
		     "337cb241e17bd472bd3677ff166a0800f684213c"))
 (esxml :source "elpaca-menu-lock-file" :recipe
	(:package "esxml" :fetcher github :repo "tali713/esxml" :files
		  ("esxml.el" "esxml-query.el") :source
		  "elpaca-menu-lock-file" :protocol https :inherit t
		  :depth treeless :ref
		  "affada143fed7e2da08f2b3d927a027f26ad4a8f"))
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
	      :source "elpaca-menu-lock-file" :protocol https :inherit
	      t :depth treeless :ref
	      "931b6d0667fe03e7bf1c6c282d6d8d7006143c52"))
 (flycheck :source "elpaca-menu-lock-file" :recipe
	   (:package "flycheck" :repo "flycheck/flycheck" :fetcher
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
		     "a4d782e7af12e20037c0cecf0d4386cd2676c085"))
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
		  "a58e29e70948512dbcdbb24745e3fb0a59984925"))
 (forge :source "elpaca-menu-lock-file" :recipe
	(:package "forge" :fetcher github :repo "magit/forge" :files
		  ("lisp/*.el" "docs/*.texi" ".dir-locals.el") :source
		  "elpaca-menu-lock-file" :protocol https :inherit t
		  :depth treeless :ref
		  "a31859547a1ea5e2acbab67b6b64f90134e2a156"))
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
		      "6258fe6390a7bf264a6f02813502bf83a645d872"))
 (ghub :source "elpaca-menu-lock-file" :recipe
       (:package "ghub" :fetcher github :repo "magit/ghub" :files
		 ("lisp/*.el" "docs/*.texi" ".dir-locals.el") :source
		 "elpaca-menu-lock-file" :protocol https :inherit t
		 :depth treeless :ref
		 "97a07691efad6fc16bc000a35be80d4f8dae251a"))
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
		 :source "elpaca-menu-lock-file" :protocol https
		 :inherit t :depth treeless :ref
		 "767571135e2c0985944017dc59b0be79af222ef5"))
 (go-mode :source "elpaca-menu-lock-file" :recipe
	  (:package "go-mode" :repo "dominikh/go-mode.el" :fetcher
		    github :files ("go-mode.el") :source
		    "elpaca-menu-lock-file" :protocol https :inherit t
		    :depth treeless :ref
		    "0ed3c5227e7f622589f1411b4939c3ee34711ebd"))
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
			      "8ff793b13707cb511875f56e167ff7f980a31136"))
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
				    "d3969faba28b942ae3c13e89277c292213813b89"))
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
		    :source "elpaca-menu-lock-file" :protocol https
		    :inherit t :depth treeless :ref
		    "7ed8bbcadb5229d648b194e0e4c4d261825aa91b"))
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
		 :inherit t :depth treeless :remotes
		 (("origin" :repo "kaorahi/howm")
		  ("fork" :repo "nicolaisingh/howm"))
		 :ref "36a4b970d631ff6aaf1388ca4f5115f23611cde8"))
 (ht :source "elpaca-menu-lock-file" :recipe
     (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
	       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		"doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		"lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		"docs/*.texinfo"
		(:exclude ".dir-locals.el" "test.el" "tests.el"
			  "*-test.el" "*-tests.el" "LICENSE" "README*"
			  "*-pkg.el"))
	       :source "MELPA" :protocol https :inherit t :depth
	       treeless :ref
	       "1c49aad1c820c86f7ee35bf9fff8429502f60fef"))
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
		    "8e3841c837b4b78bd72ad7f0436e919f39315a46"))
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
			   "8ab49b066bc23de731a29ef07bbafa29999e1852"))
 (jsonian :source "elpaca-menu-lock-file" :recipe
	  (:package "jsonian" :fetcher github :repo "iwahbe/jsonian"
		    :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		     "doc/*.texinfo" "lisp/*.el" "docs/dir"
		     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el"
			       "*-test.el" "*-tests.el" "LICENSE"
			       "README*" "*-pkg.el"))
		    :source "MELPA" :protocol https :inherit t :depth
		    treeless :ref
		    "513219ebb3ccdefc915715e4bf2dd6e718fabccd"))
 (know-your-http-well :source "elpaca-menu-lock-file" :recipe
		      (:package "know-your-http-well" :fetcher github
				:repo "for-GET/know-your-http-well"
				:files ("emacs/*.el") :source
				"elpaca-menu-lock-file" :protocol
				https :inherit t :depth treeless :ref
				"c916e825b0c75c2f2b6ff5ba79c981cff7de5797"))
 (llama :source "elpaca-menu-lock-file" :recipe
	(:package "llama" :fetcher github :repo "tarsius/llama" :files
		  ("llama.el" ".dir-locals.el") :source
		  "elpaca-menu-lock-file" :protocol https :inherit t
		  :depth treeless :ref
		  "6a67e4253cc02aa9ce85ef96290c95198b65d913"))
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
			:source "elpaca-menu-lock-file" :protocol
			https :inherit t :depth treeless :ref
			"4e87a899868e908a7a9e1812831d76c8d072f885"))
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
		      "d0928626b4711dcf9f8f90439d23701118724199"))
 (magit :source "elpaca-menu-lock-file" :recipe
	(:package "magit" :fetcher github :repo "magit/magit" :files
		  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi"
		   "docs/AUTHORS.md" "LICENSE" ".dir-locals.el"
		   (:exclude "lisp/magit-section.el"))
		  :source "elpaca-menu-lock-file" :protocol https
		  :inherit t :depth treeless :ref
		  "31afaddc07b2a9808178d4c59d8c896146d4dfad"))
 (magit-section :source "elpaca-menu-lock-file" :recipe
		(:package "magit-section" :fetcher github :repo
			  "magit/magit" :files
			  ("lisp/magit-section.el"
			   "docs/magit-section.texi"
			   "magit-section-pkg.el")
			  :source "elpaca-menu-lock-file" :protocol
			  https :inherit t :depth treeless :ref
			  "31afaddc07b2a9808178d4c59d8c896146d4dfad"))
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
			"bd27c57eada0fda1cc0a813db04731a9bcc51b7b"))
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
		       "0e7097051cbcedcc4da9b633406291d4052ec0e4"))
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
			  "7c20685df615469132763add897c1ca37cbe07e4"))
 (mathjax :source "elpaca-menu-lock-file" :recipe
	  (:package "mathjax" :repo
		    ("https://github.com/astoff/mathjax.el"
		     . "mathjax")
		    :files ("*" (:exclude ".git")) :source
		    "elpaca-menu-lock-file" :protocol https :inherit t
		    :depth treeless :ref
		    "db669451bbee7d2ea9872c28661c4679391b9644"))
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
			 "e74d4da7612c7a88e07f9dd3369e3b9fd36f396c"))
 (mermaid-ts-mode :source "elpaca-menu-lock-file" :recipe
		  (:package "mermaid-ts-mode" :fetcher github :repo
			    "JonathanHope/mermaid-ts-mode" :files
			    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			     "*.texinfo" "doc/dir" "doc/*.info"
			     "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			     "docs/dir" "docs/*.info" "docs/*.texi"
			     "docs/*.texinfo"
			     (:exclude ".dir-locals.el" "test.el"
				       "tests.el" "*-test.el"
				       "*-tests.el" "LICENSE"
				       "README*" "*-pkg.el"))
			    :source "MELPA" :protocol https :inherit t
			    :depth treeless :ref
			    "4f95d4544d5ca2d48ab93c0a133c48d94aed86d3"))
 (multi-term :source "elpaca-menu-lock-file" :recipe
	     (:package "multi-term" :fetcher github :repo
		       "manateelazycat/multi-term" :files
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
		       "017c77c550115936860e2ea71b88e585371475d5"))
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
			     "89f1a8df9b1fc721b1672b4c7b6d3ab451e7e3ef"))
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
			"62ce3a2dc39529c5db3516427e84b2c96b8efcfd"))
 (nixfmt :source "elpaca-menu-lock-file" :recipe
	 (:package "nixfmt" :fetcher github :repo
		   "purcell/emacs-nixfmt" :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		    "doc/*.texinfo" "lisp/*.el" "docs/dir"
		    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el"
			      "*-test.el" "*-tests.el" "LICENSE"
			      "README*" "*-pkg.el"))
		   :source "MELPA" :protocol https :inherit t :depth
		   treeless :ref
		   "0c449ae48b95ff7a02f3ccec6cf66963b8e4d677"))
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
		"b37d9380752e541db3f4b947c219ca54d50ca273"))
 (ob-chatgpt-shell :source "elpaca-menu-lock-file" :recipe
		   (:package "ob-chatgpt-shell" :fetcher github :repo
			     "xenodium/ob-chatgpt-shell" :files
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
			     "754ddf54a99bd98427c91a6c7374757026f8bd45"))
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
		      "254f2412489bbbf62700f9d3d5f18e537841dcc3"))
 (org-alert :source "elpaca-menu-lock-file" :recipe
	    (:package "org-alert" :fetcher github :repo
		      "spegoraro/org-alert" :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		       "doc/*.texinfo" "lisp/*.el" "docs/dir"
		       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		       (:exclude ".dir-locals.el" "test.el" "tests.el"
				 "*-test.el" "*-tests.el" "LICENSE"
				 "README*" "*-pkg.el"))
		      :source "MELPA" :protocol https :inherit t
		      :depth treeless :ref
		      "0bc04cea718387134c37c9fc4c22215adc3f79db"))
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
				 "bcab006ec42d7e2c92875c7170df193de2ee55f5"))
 (org-modern :source "elpaca-menu-lock-file" :recipe
	     (:package "org-modern" :repo "minad/org-modern" :fetcher
		       github :files
		       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			"*.texinfo" "doc/dir" "doc/*.info"
			"doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			"docs/dir" "docs/*.info" "docs/*.texi"
			"docs/*.texinfo"
			(:exclude ".dir-locals.el" "test.el"
				  "tests.el" "*-test.el" "*-tests.el"
				  "LICENSE" "README*" "*-pkg.el"))
		       :source "MELPA" :protocol https :inherit t
		       :depth treeless :ref
		       "16bef888cb8e0198dd26f869b85c10c4491a3444"))
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
			"4ec04e1b77dea76d7c30066ccf3200d2e0b7bee9"))
 (org-roam :source "elpaca-menu-lock-file" :recipe
	   (:package "org-roam" :fetcher github :repo
		     "org-roam/org-roam" :files
		     (:defaults "extensions/*") :source "MELPA"
		     :protocol https :inherit t :depth treeless :ref
		     "c17295134510c8759baad0e8a9f151029d5e5fe1"))
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
			  :source "elpaca-menu-lock-file" :protocol
			  https :inherit t :depth treeless :ref
			  "7a6498f442fc7f29504745649948635c7165d847"))
 (orgalist :source "elpaca-menu-lock-file" :recipe
	   (:package "orgalist" :repo
		     ("https://github.com/emacsmirror/gnu_elpa"
		      . "orgalist")
		     :branch "externals/orgalist" :files
		     ("*" (:exclude ".git")) :source
		     "elpaca-menu-lock-file" :protocol https :inherit
		     t :depth treeless :ref
		     "1f540f645eeb338019750d4dca3c056a1f5eca6d"))
 (origami :source "elpaca-menu-lock-file" :recipe
	  (:package "origami" :fetcher github :repo
		    "gregsexton/origami.el" :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		     "doc/*.texinfo" "lisp/*.el" "docs/dir"
		     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el"
			       "*-test.el" "*-tests.el" "LICENSE"
			       "README*" "*-pkg.el"))
		    :source "MELPA" :protocol https :inherit t :depth
		    treeless :ref
		    "e558710a975e8511b9386edc81cd6bdd0a5bda74"))
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
		"0c976bc0cd1b61f2e7761703f30d6731bb2984cd"))
 (ox-asciidoc :source "elpaca-menu-lock-file" :recipe
	      (:package "ox-asciidoc" :fetcher github :repo
			"yashi/org-asciidoc" :files ("ox-asciidoc.el")
			:source "elpaca-menu-lock-file" :protocol
			https :inherit t :depth treeless :ref
			"a8d49c44cc9aa8a3f384155f0ae052dbf36df00c"))
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
		    :source "elpaca-menu-lock-file" :protocol https
		    :inherit t :depth treeless :ref
		    "8748a908ee366e52539b19b863935d2ef7ebd0bf"))
 (ox-slack :source "elpaca-menu-lock-file" :recipe
	   (:package "ox-slack" :fetcher github :repo
		     "titaniumbones/ox-slack" :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		      "doc/*.texinfo" "lisp/*.el" "docs/dir"
		      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE"
				"README*" "*-pkg.el"))
		     :source "MELPA" :protocol https :inherit t :depth
		     treeless :ref
		     "bd797dcc58851d5051dc3516c317706967a44721"))
 (package-lint :source "elpaca-menu-lock-file" :recipe
	       (:package "package-lint" :fetcher github :repo
			 "purcell/package-lint" :files
			 (:defaults "data" (:exclude "*flymake.el"))
			 :source "elpaca-menu-lock-file" :protocol
			 https :inherit t :depth treeless :ref
			 "29ccfea319ee6bac4b80045aa473b3aa2b77d074"))
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
		 :source "elpaca-menu-lock-file" :protocol https
		 :inherit t :depth treeless :ref
		 "896696999dde9b6ac950ab485cb09f8701ad7626"))
 (password-store :source "elpaca-menu-lock-file" :recipe
		 (:package "password-store" :fetcher github :repo
			   "zx2c4/password-store" :files
			   ("contrib/emacs/*.el") :source
			   "elpaca-menu-lock-file" :protocol https
			   :inherit t :depth treeless :ref
			   "b5e965a838bb68c1227caa2cdd874ba496f10149"))
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
			       treeless :remotes
			       (("fork" :repo
				 "nicolaisingh/password-store-otp.el"))
			       :ref
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
		    :source "elpaca-menu-lock-file" :protocol https
		    :inherit t :depth treeless :ref
		    "b4d846d80dddb313042131cf2b8fbf647567e000"))
 (pdf-tools :source "elpaca-menu-lock-file" :recipe
	    (:package "pdf-tools" :fetcher github :repo
		      "vedang/pdf-tools" :files
		      (:defaults "README" ("build" "Makefile")
				 ("build" "server"))
		      :source "elpaca-menu-lock-file" :protocol https
		      :inherit t :depth treeless :ref
		      "30b50544e55b8dbf683c2d932d5c33ac73323a16"))
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
			  "dc0f497a67ee9b3d2e437e16023011780d0e8b05"))
 (plz :source "elpaca-menu-lock-file" :recipe
      (:package "plz" :repo
		("https://github.com/alphapapa/plz.el.git" . "plz")
		:files ("*" (:exclude ".git" "LICENSE")) :source
		"elpaca-menu-lock-file" :protocol https :inherit t
		:depth treeless :ref
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
			:source "elpaca-menu-lock-file" :protocol
			https :inherit t :depth treeless :ref
			"e9b73e81d3e1642aec682195f127a42dfb0b5774"))
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
		    :source "elpaca-menu-lock-file" :protocol https
		    :inherit t :depth treeless :ref
		    "336450ba90864dec62c8c883554d34aa18132165"))
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
		   :source "elpaca-menu-lock-file" :protocol https
		   :inherit t :depth treeless :ref
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
		     :source "elpaca-menu-lock-file" :protocol https
		     :inherit t :depth treeless :ref
		     "30d42b3401841ebbf2036840a2189bf62c18f507"))
 (reformatter :source "elpaca-menu-lock-file" :recipe
	      (:package "reformatter" :repo
			"purcell/emacs-reformatter" :fetcher github
			:files
			("*.el" "*.el.in" "dir" "*.info" "*.texi"
			 "*.texinfo" "doc/dir" "doc/*.info"
			 "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			 "docs/dir" "docs/*.info" "docs/*.texi"
			 "docs/*.texinfo"
			 (:exclude ".dir-locals.el" "test.el"
				   "tests.el" "*-test.el" "*-tests.el"
				   "LICENSE" "README*" "*-pkg.el"))
			:source "MELPA" :protocol https :inherit t
			:depth treeless :ref
			"6ac08cebafb9e04b825ed22d82269ff69cc5f87f"))
 (request :source "elpaca-menu-lock-file"
   :recipe
   (:package "request" :repo "tkf/emacs-request" :fetcher github
	     :files ("request.el") :source "elpaca-menu-lock-file"
	     :protocol https :inherit t :depth treeless :ref
	     "c22e3c23a6dd90f64be536e176ea0ed6113a5ba6"))
 (restclient :source "elpaca-menu-lock-file" :recipe
	     (:package "restclient" :repo "pashky/restclient.el"
		       :fetcher github :files ("restclient.el")
		       :source "elpaca-menu-lock-file" :protocol https
		       :inherit t :depth treeless :ref
		       "e2a2b13482d72634f8e49864cd9e5c907a5fe137"))
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
	      "dda84d38fffdaf0c9b12837b504b402af910d01d"))
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
		     :source "elpaca-menu-lock-file" :protocol https
		     :inherit t :depth treeless :ref
		     "1ca6e12f456caa1dc97c3d68597598662eb5de9a"))
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
			:source "elpaca-menu-lock-file" :protocol
			https :inherit t :depth treeless :ref
			"e2f05eed5bc7d10af0fc90891138d9581456914d"))
 (slime :source "elpaca-menu-lock-file" :recipe
	(:package "slime" :fetcher github :repo "slime/slime" :files
		  ("*.el" ("lib" "lib/hyperspec.el") "swank" "*.lisp"
		   "*.asd" "doc/slime.texi" "doc/slime.info" "doc/dir"
		   "ChangeLog" ("contrib" "contrib/*")
		   (:exclude "contrib/test" "contrib/Makefile"))
		  :source "elpaca-menu-lock-file" :protocol https
		  :inherit t :depth treeless :ref
		  "86b68025a9dcd6212ca6e27b228b42950c479ef3"))
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
			"603325ab3d1186fb10da5c2a7ec1afb88018d792"))
 (spinner :source "elpaca-menu-lock-file" :recipe
	  (:package "spinner" :repo
		    ("https://github.com/Malabarba/spinner.el"
		     . "spinner")
		    :files ("*" (:exclude ".git")) :source
		    "elpaca-menu-lock-file" :protocol https :inherit t
		    :depth treeless :ref
		    "d4647ae87fb0cd24bc9081a3d287c860ff061c21"))
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
		    "fcd37147121fabdf003a70279cf86fbe08cfac6f"))
 (tempel :source "elpaca-menu-lock-file" :recipe
	 (:package "tempel" :repo "minad/tempel" :fetcher github
		   :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		    "doc/*.texinfo" "lisp/*.el" "docs/dir"
		    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el"
			      "*-test.el" "*-tests.el" "LICENSE"
			      "README*" "*-pkg.el"))
		   :source "MELPA" :protocol https :inherit t :depth
		   treeless :ref
		   "9fa2c59660fe9473429a51872d3c00e1b95cab86"))
 (tide :source "elpaca-menu-lock-file" :recipe
       (:package "tide" :fetcher github :repo "ananthakumaran/tide"
		 :files (:defaults "tsserver") :source
		 "elpaca-menu-lock-file" :protocol https :inherit t
		 :depth treeless :ref
		 "6a35fe355f1442da34b976bf2decf008d6e4f991"))
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
		      "13daf473d5080b91bc37f40e0f3b566738707914"))
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
		   :source "elpaca-menu-lock-file" :protocol https
		   :inherit t :depth treeless :ref
		   "651e2634f01f346da9ec8a64613c51f54b444bc3"))
 (treesit-fold :source "elpaca-menu-lock-file" :recipe
	       (:package "treesit-fold" :repo
			 ("https://github.com/emacs-tree-sitter/treesit-fold"
			  . "treesit-fold")
			 :files ("*" (:exclude ".git")) :source
			 "elpaca-menu-lock-file" :protocol https
			 :inherit t :depth treeless :ref
			 "5b76926c14436b252150f88a8cd7e125ab295bcb"))
 (typing :source "elpaca-menu-lock-file" :recipe
	 (:package "typing" :fetcher github :repo "kensanata/typing"
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
		   "a2ef25dde2d8eb91bd9c0c6164cb5208208647fa"))
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
		    :source "elpaca-menu-lock-file" :protocol https
		    :inherit t :depth treeless :ref
		    "40b10f363e1f0bb31418fad6ab11ed2b0d57278a"))
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
			       :source "elpaca-menu-lock-file"
			       :protocol https :inherit t :depth
			       treeless :ref
			       "e391b52922086ac38397a3325933900b6d90f9f0"))
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
	     "f32cd7b09d518b629bfaa3eeb92b539891c6b9bc"))
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
			   "cca2b4b3cfcfea1919e1870519d79ed1a69aa5e2"))
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
		 "f99ef76c80e6fc3fcf650c4fe34e10726594a4c4"))
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
		     "8dce99ba3e5fcb5e2c3de360c4ee658055051f33"))
 (yasnippet :source "elpaca-menu-lock-file" :recipe
	    (:package "yasnippet" :repo "joaotavora/yasnippet"
		      :fetcher github :files
		      ("yasnippet.el" "snippets") :source
		      "elpaca-menu-lock-file" :protocol https :inherit
		      t :depth treeless :ref
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
		  :source "elpaca-menu-lock-file" :protocol https
		  :inherit t :depth treeless :ref
		  "9905f1be006fe02417fc6598be4990746053bbec")))
