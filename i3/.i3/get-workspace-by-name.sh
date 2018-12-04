#!/bin/sh

# Taken from the i3-support website. This is not used (yet) but i plan to use it
# in some usefull context.

i3-msg -t get_workspaces \
  | jq '.[] | select(.focused==true).name' \
  | cut -d"\"" -f2
