;; +-----------------------------------------------------------------+
;; | Packages and general stuff                                      |
;; +-----------------------------------------------------------------+
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

;; use correct PATH
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
;; (dumb-jump-mode)

;; magit
(use-package magit
    :ensure t
    :defer t)
(setq magit-completing-read-function 'ivy-completing-read)

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
    :ensure t)

;; expand-region
(use-package expand-region
    :ensure t)

;; use system clipboard
(setq x-select-enable-clipboard t)

;; delete until character
(use-package misc)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; why isn't this default?
(progn
  (delete-selection-mode 1)
  (transient-mark-mode 1)
  )
(desktop-load-default)
(desktop-read)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; quick confirmation with y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; don't make a bunch of backup files
(setq make-backup-files nil)

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
        (next-line)))

;; visuals
(blink-cursor-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 1)
(menu-bar-mode t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; (set-foreground-color "white")
;; (set-background-color "black")

(set-face-attribute 'default nil :height 110)
(setq visible-bell t)
(setq default-major-mode 'indented-text-mode)
(setq text-mode-hook 'turn-on-auto-fill)
(setq fill-column 100)

;; show vim-like empty lines
(setq-default indicate-empty-lines t)
(progn
  (define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
  (setcdr (assq 'empty-line fringe-indicator-alist) 'tilde))

;; clean trailing whitespace on save
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; show matching pairs
(use-package paren)
(show-paren-mode)

;; smooth scrolling
(use-package smooth-scrolling
    :ensure t)
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

;; replace a word from anywhere in the file with "C-c r"
(defun replace-in-buffer ()
"Replace text in whole buffer. Change OLD string to NEW string"
  (interactive)
  (save-excursion
    (replace-string (read-string "OLD string:")
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
(global-set-key [f9] 'compile)
(global-set-key [f10] 'compile)
(global-set-key [(meta g)] 'goto-line)
(global-set-key (kbd "C-q") 'er/expand-region)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-x f") 'find-file)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c r") 'replace-in-buffer)
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
(global-set-key (kbd "<C-return>") 'ivy-immediate-done)

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
