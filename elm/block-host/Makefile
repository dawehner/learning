run:
	nix-shell -p elmPackages.elm nodejs --command 'elm make src/Main.elm --output dist/main.js && node src/index.js'

shell:
	nix-shell -p elmPackages.elm elmPackages.elm-test