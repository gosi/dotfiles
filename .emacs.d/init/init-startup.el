;; Personals
(setq user-full-name my-name)
(setq user-mail-address my-email)

;; Configure Emacs's initial configuration
(blink-cursor-mode -1)
(setq inhibit-startup-message t)    ; No startup banner
(setq initial-scratch-message nil)  ; No message in scratch buffer
(tool-bar-mode -1)                  ; No toolbars
(scroll-bar-mode -1)                ; No scroll bar
(setq scroll-conservatively 100)    ; Scroll smoothly
(menu-bar-mode -1)                  ; No menu bar
(setq debug-on-error t)             ; Call the debugger
(setq vc-follow-symlinks t)         ; Follow the symlink

;; Delete the current file
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;; Rename the current file
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

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

;; Use system clipboard
(setq select-enable-clipboard t)

;; Show vim-like empty lines using "~"
(setq-default indicate-empty-lines t)
(progn
  (define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
  (setcdr (assq 'empty-line fringe-indicator-alist) 'tilde))

(provide 'init-startup)
