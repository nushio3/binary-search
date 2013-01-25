all: build

.PHONY: init build doc

init:
	cabal install --only-dependencies --enable-tests
	cabal configure --enable-tests
