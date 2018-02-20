#!/bin/bash
# For details got to: https://github.com/Mateuszd6/dmenu-music-player

if [ $# == 1 ]; then
    dmus_path=$1
    
    # If daemon is not alive, just quit.
    if [ `$dmus_path --query-is-daemon-alive` = "n" ]; then 
	echo ""
    else
	if [ `$dmus_path --query-is-paused` = "y" ]; then
	    full_text=" "
	else
	    full_text=" "
	fi
	
	title=`$dmus_path --query-title`
	artist=`$dmus_path --query-artist`
	full_text="$full_text$artist: $title" 
    fi

fi

echo "$full_text"
echo "$full_text"
