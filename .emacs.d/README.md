## .emacs.d

This is my emacs configuration. Settings and packages are split into
separate init-files inside the init/ folder. These are loaded by init.el!
This config is intended to just be dropped in $HOME and ready to go.

This however isn't completely dependency free. It expects:

 Dependency     |         File
--------------- | --------------------
 emacs > 24     |  `init-melpa.el`
 ag             |  `init-my-keys.el`
 mu4e           |  `init-mail.el`
 offlineimap    |  `init-mail.el`

All of these are available with a good package manager (apt-get / pacman),
or easy to build and install yourself on Linux. No idea about Windows compatibility for now (2019/01/05).

All these files are loaded so that if one fails to load the rest will continue.
This means that in theory you can end up with a subset of the intended setup.
