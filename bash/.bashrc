# .bashrc


# Custom LS colors
# LS_COLORS="no=00:fi=00:di=01;94:ln=04;00:pi=40;33:so=40;33:bd=40;33;01:cd=40;33;01:ex=00;93:*.gz=01;95:*.bz2=01;95:*.bz=01;95:*.tz=01;95:*.rpm=01;95:*.zip=01;95:*.cpio=01;95:*.t=93:*.pm=00;36:*.pod=00;36:*.conf=00;33:*.ini=00;33:*.off=00;9"
# export LS_COLORS


[ -r /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion

## Alias:
alias dir='ls --color=auto'
alias ls='ls --color=auto'
alias ll='ls -l --color=auto'
alias la='ls -a --color=auto'
alias gl='git log --oneline --branches --graph'
alias pushd='pushd > /dev/null'
alias popd='popd > /dev/null'
alias cls='clear'
alias grep='grep --color'
alias less='less -R'
alias mkdir='mkdir -p'
alias tree='tree -C'
alias df='df -h'                          # human-readable size
alias du='du -g'                          # human-readable size
alias free='free -m'                      # show sizes in MB
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
export PS1='\e[92;1m $(dir_prefix)$(p=${PWD#$HOME};((${#p}>28)) && echo "...${p:(-25)}" || echo $p) $(git_prompt)\e[0m\e[90;1mʎ\e[0m '
export PS2=' \e[90;1m  \e[0m '

export BROWSER="firefox"
export EMAIL="mateuszd7@gmail.com"
export NAME="Mateusz Dudzinski"

# TODO: Differ the defaul deamon and the one-window deamon. Visual should fire
#       framed emacs if it exists, and editor should start alternative deamon in
#       the background.
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"
export VISUAL="emacsclient -t"

# This quiets LOTS of warnings displayed when starting GTK apps.
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/lib/x86_64-linux-gnu/gtk-2.0/modules/

# TODO: No idea why when I just puts them in the .inputrc file they do not
#       work... So I put them here and there, just in case.
bind "TAB:menu-complete"
bind "set show-all-if-ambiguous on"
bind "set menu-complete-display-prefix on"

# Some shell options.
complete -cf sudo
shopt -s checkwinsize
shopt -s expand_aliases
shopt -s histappend


# Display currently used colors in the terminal.
colors() {
    local fgc bgc vals seq0

    printf "Color escapes are %s\n" '\e[${value};...;${value}m'
    printf "Values 30..37 are \e[33mforeground colors\e[m\n"
    printf "Values 40..47 are \e[43mbackground colors\e[m\n"
    printf "Value  1 gives a  \e[1mbold-faced look\e[m\n\n"

    # foreground colors
    for fgc in {30..37}; do
        # background colors
        for bgc in {40..47}; do
            fgc=${fgc#37} # white
            bgc=${bgc#40} # black

            vals="${fgc:+$fgc;}${bgc}"
            vals=${vals%%;}

            seq0="${vals:+\e[${vals}m}"
            printf "  %-9s" "${seq0:-(default)}"
            printf " ${seq0}TEXT\e[m"
            printf " \e[${vals:+${vals+$vals;}}1mBOLD\e[m"
        done
        echo; echo
    done
}


# Display a path variable in a more pleasent way.
function path() {
    echo $PATH | tr ':' '\n'
}

# Used by the propt.
dir_prefix() {
    if [ "${PWD##/home/}" != "${PWD}" ]; then
        echo -ne "~"
    fi
}

# Does nothing if there is no git in the folder. Otherwise prints the name of
# the current branch. Color is red if there are changes, white otherwise.
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

# Extract compressed folder, no matter what it is.
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

# Clear the system cache
clear_cache()
{
    sudo /bin/sh -c "free && sudo echo 3 > /proc/sys/vm/drop_caches && free"
}

# TODO: Investigate what this is!
# xhost +local:root > /dev/null 2>&1
