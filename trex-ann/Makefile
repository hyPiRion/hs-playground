## This makefile smells fishy

all: cabal.sandbox.config
	cabal install -j

cabal.sandbox.config: .extra-deps/CTRex
	cabal sandbox init
	cabal sandbox add-source .extra-deps/CTRex

.extra-deps/CTRex:
	git submodule init
	git submodule update
