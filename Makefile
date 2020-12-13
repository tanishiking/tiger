.PHONY: build
build:
	dune build

.PHONY: clean
clean:
	dune clean

.PHONY: fmt
fmt:
	dune build @fmt --auto-promote