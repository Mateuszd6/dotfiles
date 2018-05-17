# .bashrc
LS_COLORS="no=00:fi=00:di=01;94:ln=04;00:pi=40;33:so=40;33:bd=40;33;01:cd=40;33;01:ex=00;93:*.gz=01;95:*.bz2=01;95:*.bz=01;95:*.tz=01;95:*.rpm=01;95:*.zip=01;95:*.cpio=01;95:*.t=93:*.pm=00;36:*.pod=00;36:*.conf=00;33:*.ini=00;33:*.off=00;9"
export LS_COLORS

# Quiets lots of warnings displayed when starting GTK apps.
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/lib/x86_64-linux-gnu/gtk-2.0/modules/

extract () {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2) tar xvjf $1 ;;
            *.tar.gz) tar xvzf $1 ;;
            *.bz2) bunzip2 $1 ;;
            *.rar) unrar x $1 ;;
            *.gz) gunzip $1 ;;
            *.tar) tar xvf $1 ;;
            *.tbz2) tar xvjf $1 ;;
            *.tgz) tar xvzf $1 ;;
            *.zip) unzip $1 ;;
            *.Z) uncompress $1 ;;
            *.7z) 7z x $1 ;;
            *.tar.xz) tar xvfJ $1 ;;
            *) echo "'$1' cannot be extracted.";;
        esac
    else
        echo "'$1' if not a file."
    fi
}

function path() {
  echo $PATH | tr ':' '\n'
}

# Does nothing if there is no git in the folder. Otherwise prints
# the name of the current branch. Color is red if there are changes,
# white otherwise.
git_prompt()
{
    log_info=$(git branch 2>/dev/null | grep '^*' | colrm 1 2)
    if [ -n "$log_info" ]; then
        if [ -z "$(git status --porcelain)" ]; then
            echo -ne "\e[97;1m($log_info) "
        else
            echo -ne "\e[91;1m($log_info) "
        fi
    fi
}

## Alias:
alias dir='ls --color=auto'
alias ls='ls --color=auto'
alias ll='ls -l --color=auto'
alias la='ls -a --color=auto'
alias pushd='pushd > /dev/null'
alias popd='popd > /dev/null'
alias cls='clear'
alias grep='grep --color'
alias less='less -R'
alias mkdir='mkdir -p'
alias tree='tree -C'
alias ed='$EDITOR'
alias vis='$VISUAL'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'
alias .......='cd ../../../../../..'
alias ........='cd ../../../../../../..'
alias .........='cd ../../../../../../../..'
alias ..........='cd ../../../../../../../../..'
alias ...........='cd ../../../../../../../../../..'

## Display current directory:
export PS1=' \e[92;1m\w\e[0m $(git_prompt)\e[0m\e[90;1mʎ\e[0m '
export PS2=' \e[90;1m\e[0m '

export BROWSER="firefox"
export EMAIL="mateuszd7@gmail.com"
export NAME="Mateusz Dudzinski"

# TODO: Differ the defaul deamon and the one-window deamon. Visual should fire
#       framed emacs if it exists, and editor should start alternative deamon in
#       the background.
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"
export VISUAL="emacsclient -t"

# TODO: No idea why when I just puts them in the .inputrc file they do not
#       work... So I put them here and there, just in case.
bind "TAB:menu-complete"
bind "set show-all-if-ambiguous on"
bind "set menu-complete-display-prefix on"

## This code was already here:
# if [ -f /etc/bashrc ]; then
#     . /etc/bashrc
# fi
