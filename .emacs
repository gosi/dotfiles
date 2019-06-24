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

; ;; evil mode yes pls!!!
; (use-package evil
;   :ensure t
;   :init
;   (setq evil-want-C-u-scroll t
;         evil-want-keybinding nil
;         evil-auto-indent t
;         evil-normal-state-cursor 'box
;         evil-operator-state-cursor 'box
;         evil-replace-state-cursor 'box
;         evil-insert-state-cursor 'box)
;   :config
;   (evil-mode 1))
;   (global-undo-tree-mode -1)
;   ;; (turn-on-undo-tree-mode)
; (use-package evil-collection
;   :after evil
;   :ensure t
;   :config
;   (evil-collection-init))
;
; (use-package evil-escape
;   :ensure t
;   :config
;   (progn
;     (evil-escape-mode)
;     (global-set-key (kbd "<escape>") 'evil-escape))
;   :diminish evil-escape-mode)

; use correct PATH
(use-package exec-path-from-shell
  :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; ag
(use-package ag
  :ensure t)
(setq ag-highlight-search t)

;; counsel (+ ivy swiper)
(use-package counsel
  :ensure t)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

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

;; delete until character
(use-package misc)
(global-set-key (kbd "M-z") 'zap-up-to-char)


;; inhibits highlighting in specific places, like in comments
(setq ahs-inhibit-face-list '(font-lock-comment-delimiter-face
                                font-lock-comment-face
                                font-lock-doc-face
                                font-lock-doc-string-face
                                font-lock-string-face))

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

;; a comment/uncomment toggle
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (forward-line)))

;; visuals
(blink-cursor-mode 1)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; rainbow parens
(use-package rainbow-delimiters
  :ensure t)
(add-hook 'foo-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(set-cursor-color "red")
(set-foreground-color "black")
(set-background-color "white")

;; Set default font
(set-face-attribute 'default nil
                    :family "Monaco"
                    :height 120
                    :weight 'normal
                    :width 'normal)
(setq visible-bell nil)
(setq major-mode 'indented-text-mode)
(setq text-mode-hook 'turn-on-auto-fill)
(setq fill-column 100)

;; show vim-like empty lines using "~"
(setq-default indicate-empty-lines t)
(progn
  (define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
  (setcdr (assq 'empty-line fringe-indicator-alist) 'tilde))

;; clean trailing whitespace on save
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; show matching pairs
;; (use-package paren)
;; (show-paren-mode)

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
(global-set-key [(meta g)] 'goto-line)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-q")   'er/expand-region)
(global-set-key (kbd "C-x f") 'find-file)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c c")   'compile)
(global-set-key (kbd "C-c e")   'eshell)
(global-set-key (kbd "C-c r") 'replace-in-buffer)
(global-set-key (kbd "C-c j") 'dumb-jump-go)
(global-set-key (kbd "C-c t") 'dumb-jump-back)
(global-set-key (kbd "C-c q") 'dumb-jump-quick-look)
(global-set-key (kbd "C-c o") 'dumb-jump-other-window)
;; Ivy-based interface to standard commands
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;; Ivy-based interface to shell and system tools
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c p") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))


(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

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
(global-set-key [C-backspace] #'er-switch-to-previous-buffer)
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

;; this centralises the backup files instead of having them laying around being annoying
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
    backup-by-copying t    ; Don't delink hardlinks
    version-control t      ; Use version numbers on backups
    delete-old-versions t  ; Automatically delete excess backups
    kept-new-versions 6    ; how many of the newest versions to keep
    kept-old-versions 2    ; and how many of the old
    )
