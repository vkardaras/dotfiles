#!/bin/bash

# ===== CONFIG =====
ROFI="rofi -dmenu -i -theme material.rasi -p Power"

power_menu() {
    echo -e "⏻ Shutdown\n🔄 Reboot\n🔒 Lock"
}

choice=$(power_menu | $ROFI)

case "$choice" in
    "⏻ Shutdown") systemctl poweroff ;;
    "🔄 Reboot") systemctl reboot ;;
    "🔒 Lock") swaylock -c 000000 ;;
esac
