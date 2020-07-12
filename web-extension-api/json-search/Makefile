format:
	nix-shell --command 'elm-format src --yes'

build: format
	nix-shell --command 'elm make src/Main.elm --output elm.js'

build-debug: format
	nix-shell --command 'elm-live src/Main.elm --debug'