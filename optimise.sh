
# elm make src/Main.elm --optimize --output site/main.js
elm-optimize-level-2 src/Main.elm --output site/main.js

uglifyjs site/main.js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" > Main.min.js

uglifyjs main.min.js --mangle > site/main.js
