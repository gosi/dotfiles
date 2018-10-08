#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi
export BROWSER=xdg-open

xrdb ~/.Xresources
xmodmap ~/.Xmodmap
xbindkeys
xset -b
xset m 0 0
xset r rate 190 100

# keyboard layout customization
setxkbmap -layout us -option caps:escape
setxkbmap -layout se -option caps:escape
setxkbmap -layout us,se -variant ,intl -option grp:alt_shift_toggle

# disks automounting
udiskie &

#export $(dbus-launch)

# Network Manager applet
>> /dev/null which nm-applet && nm-applet &
>> /dev/null which blueman-applet && blueman-applet &

# wm
exec dwm
#exec i3
