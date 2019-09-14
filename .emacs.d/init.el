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
;(try-require 'init-ido)
(try-require 'init-colors)     ; Themes and color settings
(try-require 'init-eshell)     ; Setup eshell
(try-require 'init-font)       ; Source Code Pro
(try-require 'init-server)     ; Setup server
(try-require 'init-startup)    ; Configure startup message and stuff like that

;; Minor modes
(try-require 'init-ace)        ; Some helpful packages for moving around
(try-require 'init-company)    ; Autocompletion with less suck
;(try-require 'init-evil)       ; Modal editing like vim
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
(try-require 'init-my-keys)       ; The minor mode for all my key shortcuts
(try-require 'init-random)     ; Random packages with no configuration
(try-require 'init-smiles)     ; Just stupid fun stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote nil))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(evil-collection-setup-minibuffer t)
 '(org-agenda-files (quote ("~/org/notes.org" "~/org/todo.org")) t)
 '(package-selected-packages
   (quote
    (moe-theme org-mode org-ac yasnippet-snippets wiki-summary which-key use-package tuareg solarized-theme sml-mode slime-company racket-mode paredit org-present neotree multiple-cursors magit lorem-ipsum lean-mode latex-preview-pane js2-mode js-comint hungry-delete highlight-symbol highlight-parentheses helm-projectile git-gutter geiser flymake-rust flycheck-package expand-region exato evil-vimish-fold evil-tutor evil-surround evil-space evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-leader evil-indent-plus evil-goggles evil-exchange evil-escape evil-collection evil-args evil-anzu elpy elm-mode dumb-jump company-math company-jedi company-auctex cargo auctex-latexmk ag ace-window ace-jump-zap))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))
