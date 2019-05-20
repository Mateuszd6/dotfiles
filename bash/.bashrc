# .bashrc

[ -r /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion

source ~/.config/user-dirs.dirs

## Alias:
alias dir='ls --color'
alias ls='ls --color -h'
alias ll='ls -l --color -h'
alias la='ls -a --color -h'
alias gs='git status'
alias gl='git log --oneline --branches --graph'
alias myip="ip address | grep -e 'inet\(.*\)\(enp0s25\|wlan0\)' | cut -d' ' -f6 | sed \"s/\/24//g\""
alias pushd='pushd > /dev/null'
alias popd='popd > /dev/null'
alias cls='clear'
alias grep='grep --color'
alias less='less -R'
alias mkdir='mkdir -p'
alias tree='tree -C'
alias df='df -h'                          # human-readable size
alias du='du -h'                          # human-readable size
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

# TODO: Get rid of this.
alias fmfdb='sqlcmd -S localhost,1433 -U SA -P 9d4!Y@fQ2HH'

## Display current directory:
export PS1='\e[92;1m $(dir_prefix)$(p=${PWD#$HOME};((${#p}>28)) && echo "...${p:(-25)}" || echo $p) $(git_prompt)\e[0m\e[90;1mʎ\e[0m '
export PS2=' \e[90;1m  ┋\e[0m '

export BROWSER="firefox"
export USER_EMAIL="mateuszd7@gmail.com"
export USER_NAME="Mateusz Dudzinski"

# TODO: Differ the defaul deamon and the one-window deamon. Visual should fire
#       framed emacs if it exists, and editor should start alternative deamon in
#       the background.
export ALTERNATE_EDITOR=""
export EDITOR='emacsclient -nw'
export VISUAL="emacsclient -nw"

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

# This thing is _great_. Taken from: github.com/hrs/dotfiles
function countpage() {
    pdf2dsc "$1" /dev/stdout | grep "Pages" | sed s/[^0-9]//g
}

# Display a path variable in a more pleasent way.
function path() {
    echo $PATH | tr ':' '\n'
}

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

man() {
    LESS_TERMCAP_md=$'\e[01;32m' \
    LESS_TERMCAP_me=$'\e[0m' \
    LESS_TERMCAP_se=$'\e[0m' \
    LESS_TERMCAP_so=$'\e[30;47m' \
    LESS_TERMCAP_ue=$'\e[0m' \
    LESS_TERMCAP_us=$'\e[01;33m' \
    command man "$@"
}

repeat_util_fail()
{
    while [ $? -eq 0 ]; do
        $@
    done
}

editor-wrapper()
{
    emacsclient -nw --eval "(mat-console-init)" $@
}

is_std_c()
{
    cat $1 | grep "This file is part of the GNU C Library." &> /dev/null
    if [ $? -eq 0 ]; then echo YES; else echo NO; fi
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
clear-cache()
{
    sudo /bin/sh -c "free && sudo echo 3 > /proc/sys/vm/drop_caches && free"
}

mateusz-format()
{
    FORMATED_FILES=0
    for f in *.c *.h *.cpp *.hpp *.cc ./**/*.c ./**/*.h ./**/*.cpp ./**/*.hpp ./**/*.cc; do
        if [ -f $f ] ; then
            read -p "Format file $f? [y/N] " -r
            if [[ $REPLY =~ ^[Yy]$ ]]; then
                clang-format -style="{BasedOnStyle: mozilla, TabWidth: 4, IndentWidth: 4, BreakBeforeBraces: Allman, ColumnLimit: 80}" $f -i
                FORMATED_FILES=$((FORMATED_FILES+1))
            fi
        fi
    done

    echo "$FORMATED_FILES were formatted."
}

to-clip()
{
    if [ $# -ge 1 ] && ([ $1 == '--remove-newlines' ] || [ $1 == '-rm-nl' ]); then
        cat | tr -d '\n' | xclip -sel clip
    else
        cat | xclip -sel clip
    fi
}

# TODO: Investigate what this is!
# xhost +local:root > /dev/null 2>&1
