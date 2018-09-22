#!/bin/bash
# File: setup.sh
# Date: Fri Sep 21 21:39:50 CEST 2018
# Author: Eric Jansson <ericjansson510@gmail.com>

PACKAGES="
git
zsh
vim
tmux
htop
mutt
i3
ranger
build-essential
exuberant-ctags
g++
python3
python-pip
google-chrome-stable
okular
gedit
curl
wget
"

BACKUP_DIR=$HOME/dotfiles_old
DOTFILES_DIR=$HOME/dev/dotfiles
FILES=".vimrc .bashrc .zshrc .tmux.conf .emacs .ideavimrc"
CFG_FILES="i3"

function update_and_install() {
echo "Updating packages..."
sudo apt-get update

echo "Installing packages ($PACKAGES)"
sudo apt-get install $PACKAGES

# YouCompleteMe
sudo apt-get install build-essential cmake
sudo apt-get install python-dev python3-dev
cd ~/.vim/bundle/YouCompleteMe
./install.py --clang-completer

# Vim
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
vim +PluginInstall +qall

# fzf
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install

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

# the same as above but for $HOME/.config files
for file in $CFG_FILES; do
    echo "Moving any existing files from $HOME/.config to $BACKUP_DIR"
    mv ~/.config/$file ~/dotfiles_old/
    echo "Creating symlink to $file in $HOME/.config directory..."
    ln -sf $DOTFILES_DIR/$file ~/.config/$file
done
}
function zsh() {
# change shell to zsh (requires a restart to take effect)
which zsh
chsh -s `which zsh`
}

while true; do
    read -p "Do you want to update and install packages? " yn
    case $yn in
        [Yy]* ) update_and_install; break;;
        [Nn]* ) break;;
        * ) echo "Please answer Y/N ";;
    esac
done

while true; do
    read -p "Do you want to create symlinks? " yn
    case $yn in
        [Yy]* ) create_symlinks; break;;
        [Nn]* ) break;;
        * ) echo "Please answer Y/N ";;
    esac
done
