.PHONY: repl
repl:
	 ${MAKE} user.scm

.PHONY: *.scm
*.scm:
	 nix-shell -p mitscheme -p rlwrap --command "rlwrap -c mit-scheme --load $@"
