#!/usr/bin/env bash
set -euo pipefail

write_profile_setting() {
    dconf write /org/gnome/terminal/legacy/profiles:/:$1/$2 "${3}"
}

write_global_setting() {
    dconf write /org/gnome/terminal/legacy/$1 "${2}"
}

echo "First, create a new profile in gnome-terminal. Press enter when done."
read

dconf dump /org/gnome/terminal/legacy/

echo -e "\n\nPlease enter the uuid of your new profile: "
read -r uuid

write_global_setting default-show-menubar false
write_global_setting theme-variant "'dark'"

write_profile_setting $uuid foreground-color "'rgb(238,238,236)'"
write_profile_setting $uuid palette "['rgb(38,42,51)', 'rgb(204,87,93)', 'rgb(90,200,35)', 'rgb(255,220,85)', 'rgb(33,109,199)', 'rgb(148,153,255)', 'rgb(34,186,191)', 'rgb(255,255,255)', 'rgb(47,52,63)', 'rgb(239,41,41)', 'rgb(77,154,38)', 'rgb(255,203,0)', 'rgb(82,148,226)', 'rgb(145,113,249)', 'rgb(27,148,152)', 'rgb(225,239,255)']"
write_profile_setting $uuid scroll-on-output false
write_profile_setting $uuid use-theme-colors false
write_profile_setting $uuid background-color "'rgb(56,60,74)'"
write_profile_setting $uuid audible-bell false
write_profile_setting $uuid scrollback-lines 20000
write_profile_setting $uuid scrollbar-policy "'never'"

