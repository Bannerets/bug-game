.PHONY: default
default: build

.PHONY: build
build:
	dune build

.PHONY: install
install:
	dune install

.PHONY: uninstall
uninstall:
	dune uninstall

.PHONY: clean
clean:
	dune clean

.PHONY: lock
lock:
	opam lock ./bug-game.opam
