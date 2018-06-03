all :  build

build : 
	cabal new-build -w ghc-8.4.3

install : build
	cp `cabal-plan list-bin darkmatter` ~/.local/bin
