#!/bin/bash

# This script is used to updae my dotfiles repo so i can edit them in
# different places and have only one repo with them, and when I want
# to commit changes I just run this script in the root of the
# directory.

if [[ $PWD = $HOME ]]; then
    echo "It is not good idea to run this script in a home direcotory..."
    exit 1
fi

## EMACS STUFF:
package_list=`cat ./emacs/repo-packages-list`
rm -rf ./emacs/
mkdir -p ./emacs/
# Clone the .emacs file.
cp ~/.emacs ./emacs/
# Put extensions I've installed manually to the list.
mkdir -p ./emacs/emacs.d/
cp -r ~/.emacs.d/lisp/ ./emacs/emacs.d/

# NOTE: List of instlled repo packages must be updated manually because
#       i haven't figured out how to do it via bash.
echo -e "Please remember to update file 'emacs/installed-elpa-packages'\n"\
     "by querying emacs about variable: 'package-activated-list'!"
echo $package_list > ./emacs/repo-packages-list
more ./emacs/repo-packages-list

## I3 STUFF:
rm -rf ./i3/
mkdir -p i3/
cp -r ~/.i3/ ./i3/
