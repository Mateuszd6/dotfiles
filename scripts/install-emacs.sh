#!/bin/sh
mv -f $HOME/.emacs.d old-dotfiles/.emacs.d
mv -f $HOME/.emacs old-dotfiles/

# Get some wiki packages that can't be installed through melpa any more.
mkdir -p $HOME/.emacs.d
mkdir -p $HOME/.emacs.d/lisp/

# Copy the config file.
cp emacs/init.el $HOME/.emacs.d/init.el

# Move alias emacs to .local/bin
# This assumes .local/bin is in the path.
cp emacs/emacs-client $HOME/.local/bin/emacs

# Install sensible defaults by HRS:
git clone https://github.com/hrs/sensible-defaults.el ./tempdir/sensible-defaults.el/
cp -f ./tempdir/sensible-defaults.el/sensible-defaults.el $HOME/.emacs.d/lisp/sensible-defaults.el

# get dired+.el
cd ./tempdir
wget https://www.emacswiki.org/emacs/download/dired%2b.el # dired+
cd ..
mv ./tempdir/dired+.el $HOME/.emacs.d/lisp/

# Now start emacs which will download everything, byte compile lisp folder and
# then close automatically.x
/usr/bin/emacs --eval '(progn
                (byte-compile-file ".emacs.d/lisp/dired+.el")
                (byte-compile-file ".emacs.d/lisp/sensible-defaults.el")
                (kill-emacs))'
