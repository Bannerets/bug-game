
[Bug (2017) by Nick Bentley](https://boardgamegeek.com/boardgame/240835/bug)

The source code of the implementation: [src/bug.ml](src/bug.ml)

---

Installation:

1. Clone the repository
2. Install opam (e.g. using a package manager)
3. Run `opam install . --deps-only`
4. Run `dune build`
5. The binary file will be in `_build/install/default/bin/bug-game`, you can
copy it into the system with `dune install` (see also the `--prefix` option)
