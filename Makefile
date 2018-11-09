all :  build

build : 
	cabal new-build -w ghc-8.4.4

install : build
	cp `cabal-plan list-bin darkmatter` ~/.local/bin
