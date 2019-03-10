#!/bin/bash


WIRE=$(nmcli -f capabilities.carrier-detect,capabilities.speed device show enp0s25  \
           | awk '{print $2}'                                                       \
           | head -n 1)

if [ $WIRE == "yes" ]
then
    echo "[] Ethernet"
    echo "[] Ethernet"
    echo "#00FF00"
else
    ./wifi.sh
fi
