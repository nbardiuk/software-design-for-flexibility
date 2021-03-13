.PHONY: repl
repl:
	 ${MAKE} user.scm

.PHONY: *.scm
*.scm:
	 nix-shell -p mitscheme --command "mit-scheme --load $@"
