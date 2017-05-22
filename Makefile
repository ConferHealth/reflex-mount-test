
.PHONY: build build-fast update-build myawesomeclient.cabal

build-fast:
	nix-shell -A ghc --run 'cabal build'

build: update-build
	nix-shell -A ghcjs --run 'cabal configure --ghcjs && cabal build'

update-build: package.nix

package.nix: myawesomeclient.cabal
	rm -f package.nix
	nix-shell -p cabal2nix --run 'cabal2nix .' > package.nix

myawesomeclient.cabal: package.yaml
	nix-shell -p haskellPackages.hpack --run 'hpack'

clean:
	nix-shell --run 'cabal clean'
