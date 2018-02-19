#!/bin/bash
focused=$(xprop -root _NET_ACTIVE_WINDOW | awk -F' ' '{print $NF}')
if xprop -id "${focused}" I3_FLOATING_WINDOW | grep -q "not found"; then
    i3 "floating enable, border normal"
else
    i3 "floating disable, border pixel 1"
fi
