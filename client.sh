#!/bin/sh

# use this to run individual ghci call nicely. -Harley
# we can adopt this to test other parts separately too.
ghc -o Client -XMultiParamTypeClasses -XFlexibleInstances -XTypeSynonymInstances -XArrows -threaded Client.hs --make
if [ $? == 0 ];
 then
    # this is so that it wont spits permission complains at you if it doesn't compile so you can see source code -Harley
    ./free 2> /dev/null
    touch log/client/$(whoami)
    ./Client $1
fi
