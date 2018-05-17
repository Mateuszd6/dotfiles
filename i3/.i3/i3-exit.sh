#!/bin/bash

# Mateusz
while [ "$select" != "Logout" -a "$select" != "Shut Down" -a "$select" != "Reboot" -a "$select" != "Suspend" -a "$select" != "Hibernate" -a "$select" != "Lock" ]; do
    select=$(echo -e 'Lock\nLogout\nSuspend\nHibernate\nReboot\nShut Down' | dmenu -nb '#151515' -nf '#999999' -sb '#f00060' -sf '#000000' -i -p "Exit session?")
    [ -z "$select" ] && exit 0
done

if [ "$select" = "Logout" ]; then
    i3-msg exit
elif [ "$select" = "Shut Down" ]; then
    /usr/bin/systemctl poweroff
elif [ "$select" = "Reboot" ]; then
    /usr/bin/systemctl reboot
elif [ "$select" = "Suspend" ]; then
    /usr/bin/systemctl suspend
elif  [ "$select" = "Hibernate" ]; then
    /usr/bin/systemctl hibernate
elif [ "$select" = "Lock" ]; then
    i3lock-fancy
else
        echo "Bad commnad."
fi
