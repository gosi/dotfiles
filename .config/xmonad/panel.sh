#!/bin/bash

. panel_colors

[ -e /tmp/PANEL_FIFO ] && rm /tmp/PANEL_FIFO
mkfifo /tmp/PANEL_FIFO

MUSICICON='(/usr/bin/panel.d/music.xbm)'
MAILICON='(/usr/bin/panel.d/mail1.xbm)'
BATICON='(/usr/bin/panel.d/battery2.xbm)'
VOLICON='(/usr/bin/panel.d/spkr1.xbm)'
CALICON='(/usr/bin/panel.d/cal.xbm)'
SCORPIO='(/usr/bin/panel.d/scorpio.xbm)'
LAYOUT_SWAP1='(/usr/bin/panel.d/grid.xbm)'
LAYOUT_SWAP2='(/usr/bin/panel.d/layout_mirror.xbm)'

COLOR1="#442B0D"	#Brown
COLOR2="#A8AB68"	#Lt. Brown
COLOR3="#44CC08"	#Lime Green
COLOR4="#0000CC"	#Bright Blue
###


function mocinfo()
{
	while true
    File="`mocp -Q %file`"
    Status="`mocp -Q %state`"
   	Title="`mocp -Q %song`"
    Artist="`mocp -Q %artist`"
    Album="`mocp -Q %album`"
	do
		echo "F^fg(#ffffff)^bg(#442B0D)^i$MUSICICON $Artist: $Title | $Album"	
		sleep 2
	done
}

###


function inbox()
{
		while true
		do
			echo "GMail: ^fg(#A7A7E8)^bg($COLOR1)^i$MAILICON"
			sleep 2
		done
}

function battery_state() 
{
#if [ "$(cat /sys/class/power_supply/BAT0/status)" = "Charging" ]; then
#	echo "Charging"
#else
        while true
        do
                bat=$(cat /sys/class/power_supply/BAT0/capacity)
				echo "B^fg($COLOR2)^bg($COLOR1) ^i$BATICON:${bat}% "
                sleep 2
        done 
#fi
}

function panel_volume()
{
		while true
		do
        volLevel=$(amixer get Master | tail -n 1 | cut -d '[' -f 2 | sed 's/%.*//g')
         
		echo "V^fg($COLOR2)^bg($COLOR1)^i$VOLICON:$volLevel%"
                sleep 3
        done 
}

function logo()
{
	while true
	do
	##OS=$(uname -a | awk '{print $2}')
	echo "Z^fg(#ffffff)^bg($COLOR1)^i$LAYOUT_SWAP1"
		sleep 3
	done
}


function time_date() {
	while true
	do
		klocka=$(date +'%a %d %b %I:%M '^i$CALICON )
		echo "C${klocka}"
		sleep 4
	done 
}



bspc control --subscribe > /tmp/PANEL_FIFO &
xtitle -sf "T%s" > /tmp/PANEL_FIFO &
inbox > /tmp/PANEL_FIFO &
panel_volume > /tmp/PANEL_FIFO &
battery_state >  /tmp/PANEL_FIFO &
time_date >  /tmp/PANEL_FIFO &
mocinfo >  /tmp/PANEL_FIFO &
logo > /tmp/PANEL_FIFO &

        PANEL_HEIGHT=15
        PANEL_FONT_FAMILY='dejavusansmono'      #'inconsolata'         #'source code pro:medium'
        PANEL_FONT_SIZE=12

cat  /tmp/PANEL_FIFO | panel_dzen2 -f "$PANEL_FONT_FAMILY" -s "$PANEL_FONT_SIZE" | dzen2 -h $PANEL_HEIGHT \
-dock -ta l -title-name panel -fn "${PANEL_FONT_FAMILY}:pixelsize=${PANEL_FONT_SIZE}" -fg "$COLOR_FOREGROUND" \
-bg "$COLOR_BACKGROUND"