;; Can't leave home without it
(require-package 'cl)

;; Delete highlighted regions like normal editors
(delete-selection-mode 1)

;; Useful for large chunks of editing
(require-package 'lorem-ipsum)

;; For presenting org-files
(require-package 'org-present)

;; For JavaScript
(require-package 'js2-mode)

;; Make scheme coding much more fun
(require-package 'geiser)
(add-hook 'scheme-mode 'geiser-mode)

;; Update buffers automatically
(global-auto-revert-mode 1)

;; Expand region is lovely
(require-package 'expand-region)

;; Rust
(require-package 'cargo)
(require-package 'rust-mode)

;; Elm mode
(require-package 'elm-mode)

;; Git gutter
(require-package 'git-gutter)
(global-git-gutter-mode)

;; Highlight the symbol under the point
(require-package 'highlight-symbol)
(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
(setq highlight-symbol-on-navigation-p t)

;; Anzu mode
(require-package 'anzu)
(global-anzu-mode 1)

;; Multiple cursors ala Sublime
(require-package 'multiple-cursors)

;; For CL
(require-package 'slime)
(setq inferior-lisp-program "/usr/bin/clisp")

;; Restore window configurations
(winner-mode)

;; Yasnippet
(require-package 'yasnippet)
(require-package 'yasnippet-snippets)
(add-hook 'after-init-hook 'yas-global-mode 1)
(setq yas-triggers-in-field t)

;; which-key
(require-package 'which-key)
(which-key-mode)

;; lean support
(require-package 'lean-mode)

;; My stuff
(require-package 'wiki-summary)

;; dumb-jump
(require-package 'dumb-jump)

(require-package 'visual-regexp)
(provide 'init-random)
