GHC_N_OPTS = ghc --make -XMultiParamTypeClasses -XArrows -XTypeSynonymInstances -XFlexibleInstances -outputdir obj


all0: server client copyToBin

all: yampa glfw server client copyToBin

server:
	$(GHC_N_OPTS) Server.hs

client:
	$(GHC_N_OPTS) Client.hs

ns: yampa glfw
	$(GHC_N_OPTS) NoServer.hs

yampa:
	cd Yampa; cabal install -p

glfw:
	cabal install GLFW -p

copyToBin:
	cp Client ../hamBin/; cp Server ../hamBin/

#Makefile is generated by ghc --make so we just delete it
clean:
	rm -r Client Server NoServer obj/

