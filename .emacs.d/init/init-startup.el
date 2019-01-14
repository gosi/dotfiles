;; Personals
(setq user-full-name my-name)
(setq user-mail-address my-email)

;; Configure Emacs's initial configuration
(setq inhibit-startup-message t)    ; No startup banner
(setq initial-scratch-message nil)  ; No message in scratch buffer
(tool-bar-mode -1)                  ; No toolbars
(scroll-bar-mode -1)                ; No scroll bar
(setq scroll-conservatively 100)    ; Scroll smoothly
(menu-bar-mode -1)                  ; No menu bar
(setq debug-on-error t)             ; Call the debugger

;; Make title bar show host and file directory in a clean fashion
(setq-default frame-title-format
              '(:eval
                (format "Emacs [%s@%s]: %s %s"
                        (or (file-remote-p default-directory 'user)
                            user-real-login-name)
                        (or (file-remote-p default-directory 'host)
                            system-name)
                        (buffer-name)
                        (cond
                         (buffer-file-truename
                          (concat "(" buffer-file-truename ")"))
                         (dired-directory
                          (concat "{" dired-directory "}"))
                         (t
                          "[no file]")))))
;; Focus on pop-up frame
(add-to-list 'display-buffer-alist
             '("*Apropos*" display-buffer-same-window))
(add-to-list 'display-buffer-alist
             '("*Help*" display-buffer-same-window))

;; Ensure that Emacs doesn't pause often for GC
(setq gc-cons-threshold 100000000)

;; Make sure to allow newlines
(setq require-final-newline t)

;; Shorten the prompts to y or n.
(fset 'yes-or-no-p 'y-or-n-p)

;; Request permission before closing emacs.
(setq confirm-kill-emacs 'y-or-n-p)

;; Allow tramp to save passwords
(setq password-cache-expiry nil)

;; Never use dialog boxes
(setq use-dialog-box nil)

;; Show vim-like empty lines using "~"
(setq-default indicate-empty-lines t)
(progn
  (define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
  (setcdr (assq 'empty-line fringe-indicator-alist) 'tilde))

(provide 'init-startup)
