build:
	cabal build

install:
	cabal install --install-method=copy --installdir=$$HOME/.local/bin

.PHONY: build install

