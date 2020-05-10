public/index.html: public/elm.js

public/elm.js: $(wildcard src/*.elm)
	elm make src/Main.elm --output=public/elm.js

.PHONY=clean
clean:
	rm public/elm.js

.PHONY=dev
dev:
	npx elm-live src/Main.elm --port=8001 --start-page=public/index.html -- --output=public/elm.js
