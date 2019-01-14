#!/bin/bash
# File: setup.sh
# Date: Fri Sep 21 21:39:50 CEST 2018
# Author: Bo Eric Ramsell Jansson <bo.eric.rj@gmail.com>

PACKAGES=(
git
zsh
vim
emacs
tmux
htop
mutt
i3
i3lock
compton
confy
feh
fonts-font-awesome
mpv
vlc
automake
make
rxvt-unicode
libgtk-3-dev
scrot
ranger
nemo
j4-dmenu-desktop
build-essential
exuberant-ctags
g++
python3
python-pip
python3-udiskie
google-chrome-stable
okular
gedit
gnome-tweak-tool
virtualbox
unclutter
redshift-gtk
alsa-utils
anki
curl
wget
wajig
)

BACKUP_DIR=$HOME/dotfiles_old
DOTFILES_DIR=$HOME/workspace/dotfiles
FILES="bin .config .vimrc .bashrc .zshrc .tmux.conf .ideavimrc .xsession .xinitrc .Xmodmap .Xresources
.nvidia-settings-rc .htoprc .gtkrc-2.0 .gtkrc-2.0-mine .conkyrc .cvimrc .compton.conf .urxvt .tmux .vim .emacs.d
.xprofile"
CFG_FILES="i3 gtk-2.0 gtk-3.0 htop python redshift"

function update_and_install() {

echo "Updating packages..."
sudo apt-get update

echo "Installing packages ($PACKAGES)"
sudo apt-get install $PACKAGES

# Arc theme
git clone https://github.com/horst3180/arc-theme --depth 1 && cd arc-theme
./autogen.sh --prefix=/usr
sudo make install

# YouCompleteMe
sudo apt-get install build-essential cmake
sudo apt-get install python-dev python3-dev
cd ~/.vim/bundle/YouCompleteMe
./install.py --clang-completer

# fzf
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install

# set terminal emulator
sudo update-alternatives --config x-terminal-emulator

# change shell to zsh (requires a restart to take effect)
which zsh
chsh -s `which zsh`

}

function create_symlinks() {

# create dotfiles_old in $HOME
echo -n "Creating $BACKUP_DIR for backup of any existing dotfiles in $HOME... "
mkdir -p $BACKUP_DIR
echo "done"

# change to the dotfiles directory
echo -n "Changing to the $DOTFILES_DIR directory... "
cd $DOTFILES_DIR
echo "done"

# move any existing dotfiles in $HOME to dotfiles_old directory, then create symlinks from the files specified in $FILES in the $DOTFILES_DIR directory
for file in $FILES; do
    echo "Moving any existing dotfiles from $HOME to $BACKUP_DIR"
    mv ~/.$file ~/dotfiles_old/
    echo "Creating symlink to $file in home directory..."
    ln -sf $DOTFILES_DIR/$file ~/$file
done

# the same as above but for $HOME/.config
for file in $CFG_FILES; do
    echo "Moving any existing files from $HOME/.config to $BACKUP_DIR"
    mv ~/.config/$file ~/dotfiles_old/
    echo "Creating symlink to $file in $HOME/.config directory..."
    ln -sf $DOTFILES_DIR/.config/$file ~/.config/$file
done

}

while true; do
    read -p "Do you want to update and install packages? [Y/N] " yn
    case $yn in
        [Yy]* ) update_and_install; break;;
        [Nn]* ) break;;
        * ) echo "Please answer Y/N ";;
    esac
done

while true; do
    read -p "Do you want to create symlinks? [Y/N] " yn
    case $yn in
        [Yy]* ) create_symlinks; break;;
        [Nn]* ) break;;
        * ) echo "Please answer Y/N ";;
    esac
done
