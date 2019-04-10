DOTFILES=$HOME/workspace/dotfiles
export PATH=/usr/local/bin:/usr/bin:/bin
source $DOTFILES/.aliases
source $DOTFILES/.functions

# Use bash-completion, if available
[[ $PS1 && -f /usr/share/bash-completion/bash_completion ]] && \
    . /usr/share/bash-completion/bash_completion

# Unique Bash version check
if ((BASH_VERSINFO[0] < 4))
then
  echo "Looks like you're running an older version of Bash."
  echo "You need at least bash-4.0 or some options will not work correctly."
fi


# If not running interactively, don't do anything
[[ $- != *i* ]] && return

function shortpath() {
        local IFS=/ P=${PWD#?} F
        for F in $P; do echo -n /${F::1}; done
        [[ $P ]] || echo -n /
        echo -n ${F:1}
}

export PS1="[\A] \[\e[01;33m\]\u\[\e[m\]@\[\e[31;40m\]\h\[\e[m\] \[\e[36m\]\w\[\e[m\]\[\e[01;33m\]\\$\[\e[m\] "
export LANG="en_US.UTF-8"

## GENERAL OPTIONS ##

# Prevent file overwrite on stdout redirection
# Use `>|` to force redirection to an existing file
set -o noclobber

# Update window size after every command
shopt -s checkwinsize

# Automatically trim long paths in the prompt (requires Bash 4.x)
PROMPT_DIRTRIM=2

# Turn on recursive globbing (enables ** to recurse all directories)
shopt -s globstar 2> /dev/null

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob;

## SMARTER TAB-COMPLETION (Readline bindings) ##

# Perform file completion in a case insensitive fashion
set completion-ignore-case on

# Treat hyphens and underscores as equivalent
set completion-map-case on

# Display matches for ambiguous patterns at first tab press
set show-all-if-ambiguous on

# Immediately add a trailing slash when autocompleting symlinks to directories
set mark-symlinked-directories on

# Append to the history file, don't overwrite it
shopt -s histappend

# Save multi-line commands as one command
shopt -s cmdhist

# Record each line as it gets issued
PROMPT_COMMAND='history -a'

# Huge history. Doesn't appear to slow things down, so why not?
HISTSIZE=500000
HISTFILESIZE=100000

# Avoid duplicate entries
HISTCONTROL="erasedups:ignoreboth"

# Don't record some commands
export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear"

# Use standard ISO 8601 timestamp
# %F equivalent to %Y-%m-%d
# %T equivalent to %H:%M:%S (24-hours format)
HISTTIMEFORMAT='%F %T '

## BETTER DIRECTORY NAVIGATION ##

# Prepend cd to directory names automatically
shopt -s autocd 2> /dev/null
# Correct spelling errors during tab-completion
shopt -s dirspell 2> /dev/null
# Correct spelling errors in arguments supplied to cd
shopt -s cdspell 2> /dev/null

# This defines where cd looks for targets
# Add the directories you want to have fast access to, separated by colon
# Ex: CDPATH=".:~:~/projects" will look for targets in the current working directory, in home and in the ~/projects folder
CDPATH="."

# fzf
source ~/.fzf.bash

# Set vim as default editor
export VISUAL=vim
export EDITOR="vim"
