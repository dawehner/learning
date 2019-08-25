build:
	stack build

hlint:
	nix-shell -p hlint --run "hlint . --report"

dev-run: build hlint
	stack run