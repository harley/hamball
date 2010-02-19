GHC_N_OPTS = ghc -i$(srcdir) --make -XMultiParamTypeClasses -XArrows -XTypeSynonymInstances -XFlexibleInstances -Wall -fno-warn-name-shadowing -fno-warn-unused-matches -fno-warn-type-defaults -outputdir obj

srcdir=src

all0: s c

s:
	$(GHC_N_OPTS) $(srcdir)/Server.hs

c:
	$(GHC_N_OPTS) $(srcdir)/Client.hs

ns:
	$(GHC_N_OPTS) $(srcdir)/NoServer.hs

yampa:
	cd Yampa; cabal install

glfw:
	cabal install GLFW

copyToBin:
	cp Client ../hamBin/; cp Server ../hamBin/

# code printing: r is landscape, 2 is 2 pages, E for highlight
html:
	enscript -E --color -whtml -toc --output=code.html src/*.hs

ps:
	enscript -G2rE --color --output=code.ps src/*.hs

#Makefile is generated by ghc --make so we just delete it
clean:
	rm -r Client Server NoServer obj/

