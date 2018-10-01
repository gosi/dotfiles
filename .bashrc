DOTFILES=$HOME/workspace/dotfiles
source $DOTFILES/.aliases
source $DOTFILES/.functions

# fzf
source ~/.fzf.bash

# Set vim as default editor
export VISUAL=vim
export EDITOR="vim"

# Do not save duplicate lines in history
export HISTCONTROL=ignoredups

# Increase history size
export HISTSIZE=10000000
export HISTFILESIZE=$HISTSIZE

## PROMPT ##

case $(uname) in
    Linux)
	alias ls="ls --color=auto -FC"
	;;
    *)
	alias ls="ls -FGC"
	;;
esac

export PS1="\${debian_chroot:+(\$debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\] \[$txtcyn\]\$git_branch\[$txtred\]\$git_dirty\[$txtrst\]\$ "
