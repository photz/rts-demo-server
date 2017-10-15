dirs=src test

.PHONY: watch compile run compile-run clean test watch-test


watch:
	find websocket.2.9/ src/*.ml Makefile | grep -v '#' | entr -r make compile-run

watch-test:
	find $(dirs) | grep -v '#' | entr -r make test


compile-run: compile run

compile:
	ocamlbuild \
	-use-ocamlfind \
	-package lwt.unix \
	-package lwt.ppx \
	-package conduit \
	-package lwt \
	-package websocket \
	-package yojson \
	-package core \
	-package sqlite3 \
	-tag thread \
	-r \
	-I src/systems \
	-I src \
	src/main.native

run:
	./main.native -loglevel 3 "http://127.0.0.1:9003" 

clean:
	rm -f *~ src/*~
	ocamlbuild -clean

test:
	corebuild -I src -r test/quadtree_test.native
	./quadtree_test.native
