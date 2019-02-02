;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))

;; Package management, needed elsewhere
(require 'init-package)        ; Packaging

;; Personal information, used in other places
(try-require 'init-personal)

;; General utilities
(try-require 'init-backups)    ; Configure how emacs autosaves/backups work
(try-require 'init-browser)    ; Configure Emacs to use chrome
(try-require 'init-helm)       ; Completion and selection framework
(try-require 'init-colors)     ; Color theme
(try-require 'init-eshell)     ; Setup eshell
(try-require 'init-font)       ; Source Code Pro
(try-require 'init-server)     ; Setup server
(try-require 'init-startup)    ; Configure startup message and stuff like that

;; Minor modes
(try-require 'init-ace)        ; Some helpful packages for moving around
(try-require 'init-company)    ; Autocompletion with less suck
(try-require 'init-flycheck)   ; Syntax checking
(try-require 'init-parens)     ; Pretty parens
(try-require 'init-hippie)     ; Configure hippie
(try-require 'init-delete)     ; Hungry delete mode
(try-require 'init-whitespace) ; Judgemental highlighting!

;; Tools in Emacs
(try-require 'init-ag)         ; Ag in emacs
(try-require 'init-vcs)        ; Configuration for all things VCS
(try-require 'init-gpg)        ; For encrypting stuffs
(try-require 'init-org)        ; Setup org mode with nicities
(try-require 'init-mail)       ; Setup mu4e and smtpmail for gmail
(try-require 'init-tree)       ; Setup a directory tree

;; Language specifics
(try-require 'init-c-lang)     ; C/C++ settings
(try-require 'init-elisp)      ; Random utilities for elisp
(try-require 'init-js)         ; js-comint mode
(try-require 'init-latex)      ; Configure latex, preview pane
(try-require 'init-markdown)   ; Configure markdown
(try-require 'init-ocaml)      ; Nicer key bindings for Tuareg
(try-require 'init-python)     ; Python settings
(try-require 'init-racket)     ; Racket for when I'm nostalgic
(try-require 'init-rust)       ; Setup rust with a few helper keys
(try-require 'init-sml)        ; Setup sml with some helpers for 15-150

;; Make Emacs more like home
(try-require 'init-util-fns)   ; Some useful functions I bind to keys
(try-require 'init-my-keys)    ; The minor mode for all my key shortcuts
(try-require 'init-random)     ; Random packages with no configuration
(try-require 'init-smiles)     ; Just stupid fun stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3c3836" "#fb4933" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(custom-safe-themes
   (quote
    ("e2fd81495089dc09d14a88f29dfdff7645f213e2c03650ac2dd275de52a513de" "2a9039b093df61e4517302f40ebaf2d3e95215cb2f9684c8c1a446659ee226b9" "7f89ec3c988c398b88f7304a75ed225eaac64efa8df3638c815acc563dfd3b55" "2c7dba584d018891949689f817ce2faf96126d3be847f4b511c539f629379f23" default)))
 '(package-selected-packages
   (quote
    (evil-surround zenburn-theme gruvbox-theme evil-escape evil-collection yasnippet-snippets wiki-summary which-key use-package tuareg sml-mode smart-mode-line slime-company racket-mode paredit org-present nyan-mode neotree multiple-cursors magit lorem-ipsum lean-mode latex-preview-pane js2-mode js-comint hungry-delete highlight-symbol highlight-parentheses helm git-gutter geiser flymake-rust flycheck-package expand-region evil elm-mode distinguished-theme counsel company-math company-auctex color-theme cargo auctex-latexmk anzu ag ace-window ace-jump-zap 2048-game)))
 '(pdf-view-midnight-colors (quote ("#fdf4c1" . "#282828"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
