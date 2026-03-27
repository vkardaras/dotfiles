#!/bin/bash

# chosen=$(echo -e "Shutdown\nReboot\nLock" | rofi -dmenu -p "Power Menu")
chosen=$(echo -e " Shutdown\n Reboot\n Lock" | rofi -dmenu -p "Power")

case "$chosen" in
    Shutdown)
        systemctl poweroff
        ;;
    Reboot)
        systemctl reboot
        ;;
    Lock)
        i3lock
        ;;
esac
