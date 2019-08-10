;;; package --- Summary
;;; Commentary:

;;; Code:
(add-to-list 'load-path "~/.emacs.d/lisp")
(setq debug-on-error t)
(setq vc-follow-symlinks t)
(setq package-check-signature nil)
(setq custom-file "~/.emacs-custom")
(if (file-exists-p custom-file)
    (load custom-file))

;; setup package and use-package
(package-initialize)

;; override the default http with https
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")))

;; add melpa to the front
(add-to-list 'package-archives
            '("melpa" . "https://melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

; use correct PATH
(use-package exec-path-from-shell
  :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; ido-mode
(ido-mode 1)

;; dumb-jump
(use-package dumb-jump
  :ensure t)

;; magit
(use-package magit
    :ensure t
    :defer t)
(setq magit-completing-read-function 'ivy-completing-read)

;; git gutter
(use-package git-gutter
    :ensure t
    :config
    (global-git-gutter-mode 't)
    :diminish git-gutter-mode)

;; yasnippet
(use-package yasnippet
    :ensure t)
(add-hook 'after-init-hook 'yas-global-mode 1)

(use-package yasnippet-snippets
    :ensure t)

;; auto close (), {}, [], ""
(electric-pair-mode 1)

;; company-mode
(use-package company
  :ensure t)
(setq yas-triggers-in-field t)
(add-hook 'after-init-hook 'global-company-mode)

;; flycheck
(use-package flycheck
    :ensure t
    :config
    (add-hook 'after-init-hook 'global-flycheck-mode)
    (add-to-list 'flycheck-checkers 'proselint)
    (setq-default flycheck-highlighting-mode 'lines))

;; expand-region
(use-package expand-region
    :ensure t)

;; use system clipboard
(setq select-enable-clipboard t)

;; tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-set-style "k&r")
(setq c-basic-offset 4)

;; why isn't this default?
(progn
(delete-selection-mode 1)
(transient-mark-mode 1))
(desktop-save-mode)
(desktop-read)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; quick confirmation with y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; focuses on pop-up frame, tap q to close and return
(add-to-list 'display-buffer-alist
             '("*Apropos*" display-buffer-same-window))
(add-to-list 'display-buffer-alist
             '("*Help*" display-buffer-same-window))

;; visuals
(blink-cursor-mode 1)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; (set-cursor-color "green")
;; (set-foreground-color "black")
;; (set-background-color "white")

;; Set default font
(set-face-attribute 'default nil
                    :family "monospace"
                    :height 110
                    :weight 'normal
                    :width 'normal)
(setq visible-bell nil)
(setq major-mode 'indented-text-mode)
(setq text-mode-hook 'turn-on-auto-fill)
(setq fill-column 100)

;; show empty lines
(setq-default indicate-empty-lines t)

;; clean trailing whitespace on save
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; smooth scrolling
(use-package smooth-scrolling
    :ensure t)
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

;; replace a word from anywhere in the file with "C-c r"
(defun replace-in-buffer ()
"Replace text in whole buffer.  Change OLD string to NEW string."
  (interactive)
  (save-excursion
    (replace-match (read-string "OLD string:")
                    (read-string "NEW string:")
                    nil
                    (point-min)
                    (point-max))))

;; +-----------------------------------------------------------------+
;; | Keyboard shortcuts and bindings                                 |
;; +-----------------------------------------------------------------+
(global-set-key [f4] 'kill-this-buffer)
(global-set-key [f5] 'eval-buffer)
(global-set-key [f8] 'delete-trailing-whitespace)
(global-set-key [f9] 'recompile)
(global-set-key [f10] 'compile)
(global-set-key (kbd "C-;")   'er/expand-region)
(global-set-key (kbd "C-?")   'help-command)
(global-set-key (kbd "C-h")   'backward-delete-char-untabify)
(global-set-key (kbd "C-o")   'other-window)
(global-set-key (kbd "C-w")   'backward-kill-word)
(global-set-key (kbd "C-x f") 'find-file)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c e") 'eshell)
(global-set-key (kbd "C-c r") 'replace-in-buffer)
(global-set-key (kbd "C-c j") 'dumb-jump-go)
(global-set-key (kbd "C-c t") 'dumb-jump-back)
(global-set-key (kbd "C-c q") 'dumb-jump-quick-look)
(global-set-key (kbd "C-c o") 'dumb-jump-other-window)
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

;; Scroll other window from the current focused one
(define-key global-map [(meta up)] '(lambda() (interactive) (scroll-other-window -1)))
(define-key global-map [(meta down)] '(lambda() (interactive) (scroll-other-window 1)))

;; Move by lines of five
(global-set-key (kbd "M-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 5))))

(global-set-key (kbd "M-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 5))))

(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))
;; quickly open this file
(defun find-config ()
    "Edit the .emacs file."
    (interactive)
    (find-file "~/.emacs"))

  (global-set-key (kbd "C-c I") 'find-config)

;; don't accidently kill emacs
(defun dont-kill-emacs ()
  (interactive)
  (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))

(global-set-key "\C-x\C-c" 'dont-kill-emacs)
(setq frame-title-format "%b - emacs")
(setq require-final-newline 't)

(defconst animate-n-steps 3)
(defun emacs-reloaded ()
  (animate-string (concat ";; Initialization successful, welcome to "
                          (substring (emacs-version) 0 16)
                          ".")
                  0 0)
  (newline-and-indent)  (newline-and-indent))
(add-hook 'after-init-hook 'emacs-reloaded)


(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)


(defun er-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
 (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key [C-tab] #'er-switch-to-previous-buffer)
;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; eshell hacks
 (defun eshell-here ()
  "Opens up a new shell in the directory associated with the
  current buffer's file. The eshell is renamed to match that
  directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "<C-M-return>") 'eshell-here)
(global-set-key (kbd "<C-M-backspace>") 'kill-buffer-and-window)

(defun eshell/clear ()
  "You can type 'clear' to remove clutter like you would expect."
  (interactive)
  (let ((eshell-buffer-maximum-lines 0)) (eshell-truncate-buffer)))

(defun eshell/x ()
  "Need to kill eshell before using the eshell-here function again, or shit gets fucked up."
  (kill-buffer-and-window))

;; Org mode settings
(setq org-src-fontify-natively t)
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

(setq org-support-shift-select 'always)

(setq org-log-done 'time)

(setq org-directory "~/Dropbox/org")
;; Capture for taking notes
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files (concat org-directory "/agenda-files.org"))

(setq org-capture-templates
      '(("a" "Appointment" entry (file+datetree "~/Dropbox/org/appointments.org")
         "* %?\n %i\n %a")
        ("t" "Todo" entry (file+headline "~/Dropbox/org/notes.org" "Tasks")
         "* TODO %?\n %i\n %a")
        ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
         "* %?\nEntered on %U\n %i\n %a")))

;;set priority range from A to C with default A
(setq org-highest-priority ?A)
(setq org-lowest-priority ?C)
(setq org-default-priority ?A)

;;set colours for priorities
(setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                           (?B . (:foreground "LightSteelBlue"))
                           (?C . (:foreground "OliveDrab"))))

;;open agenda in current window
(setq org-agenda-window-setup (quote current-window))

;; this centralises the backup files instead of having them laying around being annoying
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
    backup-by-copying t    ; Don't delink hardlinks
    version-control t      ; Use version numbers on backups
    delete-old-versions t  ; Automatically delete excess backups
    kept-new-versions 6    ; how many of the newest versions to keep
    kept-old-versions 2    ; and how many of the old
    )
