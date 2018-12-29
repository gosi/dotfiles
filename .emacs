;; +-----------------------------------------------------------------+
;; | Packages and general stuff                                      |
;; +-----------------------------------------------------------------+
(add-to-list 'load-path "~/.emacs.d/lisp")
(setq debug-on-error t)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                        ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))
(package-initialize)
(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))
;; inhibits highlighting in specific places, like in comments
(setq ahs-inhibit-face-list '(font-lock-comment-delimiter-face
                                font-lock-comment-face
                                font-lock-doc-face
                                font-lock-doc-string-face
                                font-lock-string-face))

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; auto close (), {}, [], ""
(electric-pair-mode 1)

;; company-mode
(require 'company)
(global-company-mode 1)
(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'after-init-hook 'global-company-mode)

;; use system clipboard
(setq x-select-enable-clipboard t)

;; delete until character
(require 'misc)
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
(tool-bar-mode 1)
(scroll-bar-mode 1)
(menu-bar-mode t)
(blink-cursor-mode 0)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;(set-foreground-color "#DAB98F")
;;(set-background-color "#161616")
(set-cursor-color "Lime")
(set-face-attribute 'default nil :height 120)

(setq-default indicate-empty-lines t)
(setq visible-bell t) ;;removes alarm sound and flashes screen instead
(setq default-major-mode 'indented-text-mode)
(setq text-mode-hook 'turn-on-auto-fill)
(setq fill-column 100)

;; show matching pairs
(require 'paren)
(show-paren-mode)

;; smooth scrolling
(require-package 'smooth-scrolling)
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

;; +-----------------------------------------------------------------+
;; | Keyboard shortcuts and bindings                                 |
;; +-----------------------------------------------------------------+
(global-set-key [f2] 'save-buffer)
(global-set-key [f4] 'kill-this-buffer)
(global-set-key [f8] 'delete-trailing-whitespace)
(global-set-key [f9] 'compile)
(global-set-key [f10] 'compile)
(global-set-key [(meta g)] 'goto-line)
(global-set-key (kbd "C-q") 'er/expand-region)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-x f") 'find-file)

;; make line below or above current line without breaking
(global-set-key (kbd "<C-return>") (lambda ()
                   (interactive)
                   (end-of-line)
                   (newline-and-indent)))

(global-set-key (kbd "<C-S-return>") (lambda ()
                       (interactive)
                       (beginning-of-line)
                       (newline-and-indent)
                       (previous-line)))


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
