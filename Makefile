live:
	elm-live ./src/Main.elm --start-page=./public/index.html -- --output=elm.js

build: 
	elm make src/Main.elm --optimize --output=./public/elm.js
	uglifyjs ./public/elm.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=public/elm.min.js
	gzip -f ./public/elm.min.js 
	rm ./public/elm.js

serve:
	elm-live src/Main.elm --hot --dir=./public -- --output=public/elm.min.js
