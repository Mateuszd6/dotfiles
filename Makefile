.PHONY: all init update \
		bash update-bash \
		emacs update-emacs

#
# Install dotfiles
#
all: init bash emacs passwords

init:
	rm -rf tempdir
	rm -rf old-dotfiles
	mkdir -p tempdir
	mkdir -p old-dotfiles
	mkdir -p $(HOME)/.local
	mkdir -p $(HOME)/.local/bin

bash: init
	-mv -f $(HOME)/.bashrc old-dotfiles/
	-mv -f $(HOME)/.bash_profile old-dotfiles/
	cp ./bash/bashrc $(HOME)/.bashrc
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

passwords: init
	cp passwords/mdpassmenu $(HOME)/.local/bin

#
# Update dotfiles
#
update: update-bash update-emacs

update-bash:
	cp $(HOME)/.bashrc ./bash/bashrc
	cp $(HOME)/.bash_profile ./bash/bash_profile

update-emacs:
	cp $(HOME)/.emacs.d/init.el ./emacs/init.el
