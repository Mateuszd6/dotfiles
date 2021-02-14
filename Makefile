.PHONY: all init update \
		bash update-bash \
		emacs update-emacs \
		git update-git \
		x update-x \
		scripts


#
# Install dotfiles
#
all :
	@-echo "You _don't_ want to install everything, do you?"

init:
	rm -rf tempdir
	rm -rf old-dotfiles
	mkdir -p tempdir
	mkdir -p old-dotfiles
	mkdir -p $(HOME)/.local
	mkdir -p $(HOME)/.local/bin

bash: init
	-mv -f $(HOME)/.bashrc old-dotfiles/
	-mv -f $(HOME)/.inputrc old-dotfiles/
	-mv -f $(HOME)/.bash_profile old-dotfiles/
	cp ./bash/bashrc $(HOME)/.bashrc
	cp ./bash/inputrc $(HOME)/.inputrc
	cp ./bash/bash_profile $(HOME)/.bash_profile

emacs: init
	-mv -f $(HOME)/.emacs.d old-dotfiles/.emacs.d
	-mv -f $(HOME)/.emacs old-dotfiles/

    # Get some wiki packages that can't be installed through melpa any more.
	mkdir -p $(HOME)/.emacs.d
	mkdir -p $(HOME)/.emacs.d/lisp/

    # Copy the config file.
	cp emacs/init.el $(HOME)/.emacs.d/init.el

    # Install sensible defaults by HRS:
	git clone \
	    https://github.com/hrs/sensible-defaults.el \
	    ./tempdir/sensible-defaults.el/
	cp -f ./tempdir/sensible-defaults.el/sensible-defaults.el \
	   $(HOME)/.emacs.d/lisp/sensible-defaults.el

    # get dired+.el
	wget -P ./tempdir https://www.emacswiki.org/emacs/download/dired%2b.el
	cp ./tempdir/dired+.el $(HOME)/.emacs.d/lisp/

    # Move alias emacs to .local/bin, assumes .local/bin is in the path.
	cp emacs/emacs-client $(HOME)/.local/bin/emacs

    # Now start emacs which will download everything, byte compile lisp folder
    # and then close automatically.
	/usr/bin/emacs --eval '(progn (byte-compile-file ".emacs.d/lisp/dired+.el") (byte-compile-file ".emacs.d/lisp/sensible-defaults.el") (kill-emacs))'

git: init
	-mv -f $(HOME)/.gitconfig old-dotfiles/.gitconfig
	cp ./git/gitconfig $(HOME)/.gitconfig

scripts: init
	cp scripts/displ-mail $(HOME)/.local/bin
	cp scripts/agenda.sh $(HOME)/.local/bin
	cp scripts/capture.sh $(HOME)/.local/bin
	cp scripts/grep-find-todos $(HOME)/.local/bin
	cp scripts/mdpassmenu $(HOME)/.local/bin

# This should install window manager terminal emulator and copy xinit files, so
# that startx should straight-up work.
x: init
	rm -rf dwm-customized
	rm -rf st-customized
	rm -rf dynamic-bar
	git clone https://github.com/Mateuszd6/dwm-customized.git
	git clone https://github.com/Mateuszd6/st-customized.git
	git clone https://github.com/Mateuszd6/dynamic-bar.git
	cd dwm-customized && make && sudo make install
	cd st-customized && make && sudo make install
	cd dynamic-bar && make && sudo make install
	mv -f $(HOME)/.xinitrc old-dotfiles/xinit
	mv -f $(HOME)/.Xresources old-dotfiles/Xresources
	cp x/xinit $(HOME)/.xinitrc
	cp x/Xresources $(HOME)/.Xresources

#
# Update dotfiles
#
update: update-bash update-emacs update-git update-x

update-bash:
	cp -f $(HOME)/.bashrc ./bash/bashrc
	cp -f $(HOME)/.inputrc ./bash/inputrc
	cp -f $(HOME)/.bash_profile ./bash/bash_profile

update-emacs:
	cp -f $(HOME)/.emacs.d/init.el ./emacs/init.el

update-git:
	cp -f $(HOME)/.gitconfig ./git/gitconfig

update-x:
	cp -f $(HOME)/.xinitrc ./x/xinit
	cp -f $(HOME)/.Xresources ./x/Xresources
