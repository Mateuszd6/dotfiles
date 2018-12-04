#!/bin/sh

urxvtc "$@"
if [ $? -eq 2 ]; then
    echo "Daemon is not started..."
   urxvtd -q -o -f
   urxvtc "$@"

   if [ $? -eq 2 ]; then
       echo "Could not start deamon..."
       urxvt "$@"
   fi
fi
