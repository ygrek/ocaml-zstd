build:
	dune build

doc:
	dune build @doc

test:
	dune runtest

all: build doc test

install:
	dune install

uninstall:
	dune uninstall


clean:
	dune clean

.PHONY: build doc test all install uninstall clean

VERSION=0.4
NAME=zstd-$(VERSION)

.PHONY: release
release:
	dune-release tag v$(VERSION)
	dune-release distrib
	gpg -a -b _build/$(NAME).tbz -o $(NAME).tbz.asc
	dune-release publish -m ""
	dune-release opam pkg
	dune-release opam submit -m ""
