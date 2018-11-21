[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources
urxvtd -q -o -f
exec i3
