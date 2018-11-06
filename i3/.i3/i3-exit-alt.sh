#!/bin/bash

RUNNING=true

while $RUNNING; do
    SELECT=$(echo -e "Lock\nLogout\nSuspend\nHibernate\nReboot\nShut Down" | \
                 /home/mateusz/work/aluncher/bin/program --dmenu --sb \#930525)

    case $SELECT in
        "Logout") i3-msg exit; RUNNING=false ;;
        "Shut Down") /usr/bin/systemctl poweroff; RUNNING=false ;;
        "Reboot") /usr/bin/systemctl reboot; RUNNING=false ;;
        "Suspend") /usr/bin/systemctl suspend; RUNNING=false ;;
        "Hibernate") /usr/bin/systemctl hibernate; RUNNING=false ;;
        "Lock") i3lock-fancy; RUNNING=false ;;
        "") exit 0
    esac;
done
