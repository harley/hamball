#!/bin/sh

if [ $? == 0 ];
 then
    touch log/server/$(whoami)
    # this writes the hostname to a file name .host_name
    ./Server $(hostname)
    rm .host_name
fi

