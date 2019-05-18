#!/bin/bash

RUNNING=true

while $RUNNING; do
    SELECT=$(echo -e "Lock\nLock the screen\nLogout\nExit the session\nSuspend\nSuspend to ram\nHibernate\nHibernate to disc\nReboot\nRestart the machine\nShut Down\nPower off the machine" |                             \
                 mdmenu -l 6 -topof 23 -winpad 2 -texthpad 5 -textvpad 3           \
                        -mainfn "DejaVu Sans Mono-18"                              \
                        -entryfn "DejaVu Sans Mono-12"                             \
                        -commfn "DejaVu Sans Mono-8"                               \
                        -dmenu -nb \#272822 -nf \#F6F6F6 -sb \#930525 -sf \#F6F6F6 \
                        -mc \#1E1F1C -disp-comments)

    case $SELECT in
        "Logout") i3-msg exit; RUNNING=false ;;
        "Shut Down") /usr/bin/systemctl poweroff; RUNNING=false ;;
        "Reboot") /usr/bin/systemctl reboot; RUNNING=false ;;
        "Suspend") /usr/bin/systemctl suspend; RUNNING=false ;;
        "Hibernate") /usr/bin/systemctl hibernate; RUNNING=false ;;
        "Lock") i3lock --image=/home/mateusz/Pictures/wallpaper.png -f ; RUNNING=false ;;
        "") exit 0
    esac;
done
