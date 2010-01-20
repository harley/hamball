#!/bin/sh

if [ $? == 0 ];
 then
    touch log/client/$(whoami)
    ./Client $1
fi

