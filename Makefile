.PHONY: repl
repl:
	 nix-shell -p mitscheme --command "mit-scheme --load user.scm"
