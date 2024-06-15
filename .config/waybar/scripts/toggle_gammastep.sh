##!/bin/bash

if pgrep -x "gammastep" >/dev/null; then
	pkill gammastep
else
	gammastep -c ~/.config/gammastep/config.ini &
fi
