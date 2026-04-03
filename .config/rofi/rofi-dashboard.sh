#!/bin/bash

# ===== CONFIG =====
ROFI="rofi -dmenu -i -theme material.rasi -p Dashboard"

# ===== FUNCTIONS =====

main_menu() {
    echo -e "🔊 Volume\n🌐 Network\n💡 Brightness\n📊 System Info\n⏻ Power"
}

volume_menu() {
    echo -e "🔼 Volume Up\n🔽 Volume Down\n🔇 Mute"
}

network_menu() {
    echo -e "📡 WiFi Toggle\n📶 WiFi List"
}

brightness_menu() {
    echo -e "🔆 Increase\n🔅 Decrease"
}

power_menu() {
    echo -e "⏻ Shutdown\n🔄 Reboot\n🔒 Lock"
}

system_info() {
    notify-send "System Info" \
    "CPU: $(grep 'cpu ' /proc/stat)\nRAM: $(free -h | awk '/Mem/ {print $3 "/" $2}')"
}

# ===== MAIN LOGIC =====

choice=$(main_menu | $ROFI)

case "$choice" in
    "🔊 Volume")
        vol=$(volume_menu | $ROFI)
        case "$vol" in
            "🔼 Volume Up") pactl set-sink-volume @DEFAULT_SINK@ +5% ;;
            "🔽 Volume Down") pactl set-sink-volume @DEFAULT_SINK@ -5% ;;
            "🔇 Mute") pactl set-sink-mute @DEFAULT_SINK@ toggle ;;
        esac
        ;;
        
    "🌐 Network")
        net=$(network_menu | $ROFI)
        case "$net" in
            "📡 WiFi Toggle") nmcli radio wifi off || nmcli radio wifi on ;;
            "📶 WiFi List")
                wifi=$(nmcli -t -f SSID dev wifi | sort -u | rofi -dmenu -p "WiFi")
                [ -n "$wifi" ] && nmcli dev wifi connect "$wifi"
                ;;
        esac
        ;;
        
    "💡 Brightness")
        bright=$(brightness_menu | $ROFI)
        case "$bright" in
            "🔆 Increase") brightnessctl set +10% ;;
            "🔅 Decrease") brightnessctl set 10%- ;;
        esac
        ;;
        
    "📊 System Info")
        system_info
        ;;
        
    "⏻ Power")
        pow=$(power_menu | $ROFI)
        case "$pow" in
            "⏻ Shutdown") systemctl poweroff ;;
            "🔄 Reboot") systemctl reboot ;;
            "🔒 Lock") i3lock -c 000000 ;;
        esac
        ;;
esac
