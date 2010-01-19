all: server client

server:
	ghc --make -XMultiParamTypeClasses -XArrows -XTypeSynonymInstances -XFlexibleInstances Server.hs

client:
	ghc --make -XMultiParamTypeClasses -XArrows -XTypeSynonymInstances -XFlexibleInstances Client.hs

yampa:
	cabal install

clean:
	rm *.o *.hi Client Server

