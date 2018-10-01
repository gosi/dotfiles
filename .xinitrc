#!/bin/sh

# Make sure this is before the 'exec' command or it won't be sourced.
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
  for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
    [ -x "$f" ] && . "$f"
    done
  unset f
fi
xport BROWSER=xdg-open

xrdb ~/.Xresources
xmodmap ~/.Xmodmap
xbindkeys
xset -b
xset m 0 0 &
xset r rate 150 70 & 
# keyboard layout customization
setxkbmap -layout us -option caps:escape
setxkbmap -layout se -option caps:escape
setxkbmap -layout us,se -variant ,intl -option grp:alt_shift_toggle

# disks automounting
udiskie &

#export $(dbus-launch)

# background

[ -e ~/.fehbg ] && source ~/.fehbg

# Network Manager applet
>> /dev/null which nm-applet && nm-applet &
>> /dev/null which blueman-applet && blueman-applet &

# wm
i3-regenerate
exec i3
