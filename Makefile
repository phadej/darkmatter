all :  build

build : 
	cabal v2-build -w ghc-8.4.4

install :
	cabal v2-install -w ghc-8.4.4 --overwrite-policy=always
