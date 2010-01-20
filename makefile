all: yampa glfw server client

server:
	ghc --make -XMultiParamTypeClasses -XArrows -XTypeSynonymInstances -XFlexibleInstances Server.hs

client:
	ghc --make -XMultiParamTypeClasses -XArrows -XTypeSynonymInstances -XFlexibleInstances Client.hs

yampa:
	cd Yampa; cabal install

glfw:
	cabal install GLFW
#Makefile is generated by ghc --make so we just delete it
clean:
	rm *.o *.hi Client Server Makefile

