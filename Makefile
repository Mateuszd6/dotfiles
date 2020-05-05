.PHONY: all emacs

all: emacs

init:
	mkdir -p .local
	mkdir -p .local/bin

emacs:
	./scripts/install-emacs.sh

#
# Update dotfiles
#
update-emacs:
	cp $(HOME)/.emacs.d/init.el ./emacs/init.el
