.PHONY: all init update \
		emacs update-emacs

#
# Install dotfiles
#
all: init emacs

init:
	rm -rf tempdir
	rm -rf old-dotfiles
	mkdir -p tempdir
	mkdir -p old-dotfiles
	mkdir -p $(HOME)/.local
	mkdir -p $(HOME)/.local/bin

emacs: init
	./scripts/install-emacs.sh

#
# Update dotfiles
#
update: update-emacs

update-emacs:
	cp $(HOME)/.emacs.d/init.el ./emacs/init.el
