#!/bin/bash

chosen=$(echo -e "OCPI\nSmartCharging\nLocation\nSession" | rofi -dmenu -p "CPO Project")j

case "$chosen" in
    OCPI)
        /opt/idea-IU-253.32098.37/bin/idea -n /home/vkardaras/Documents/cpo-devcontainers/ppc-cpo-devcontainers/cpo-ocpi-connector-app
        ;;
    SmartCharging)
        /opt/idea-IU-253.32098.37/bin/idea -n ~/Documents/cpo-devcontainers/ppc-cpo-devcontainers/cpo-smart-charging-app/
        ;;
    Location)
        /opt/idea-IU-253.32098.37/bin/idea -n ~/Documents/cpo-devcontainers/ppc-cpo-devcontainers/cpo-location-app/
        ;;
    Session)
        /opt/idea-IU-253.32098.37/bin/idea -n ~/Documents/cpo-devcontainers/ppc-cpo-devcontainers/cpo-sessions-app/
        ;;
esac
