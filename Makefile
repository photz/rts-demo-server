dirs=src test

.PHONY: watch compile run compile-run clean test watch-test

watch-test:
	find $(dirs) | grep -v '#' | entr -r make test

watch:
	find websocket.2.9/ src/*.ml Makefile | grep -v '#' | entr -r make compile-run


compile-run: compile run

compile:
	ocamlbuild \
	-use-ocamlfind \
	-package lwt.unix \
	-package lwt.ppx \
	-package conduit \
	-package lwt \
	-package websocket.lwt \
	-package yojson \
	-package core \
	-tag thread \
	-r \
	src/main.native

run:
	./main.native -loglevel 3 "http://127.0.0.1:9003" 

clean:
	rm -f *~ src/*~
	ocamlbuild -clean

test:
	corebuild -I src -r test/quadtree_test.native
	./quadtree_test.native
