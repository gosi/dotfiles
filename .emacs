;; +-----------------------------------------------------------------+
;; | Packages and general stuff                                      |
;; +-----------------------------------------------------------------+
(add-to-list 'load-path "~/.emacs.d/lisp")
(setq debug-on-error t)
(setq package-check-signature nil)

;; setup package and use-package
(package-initialize)

;; override the default http with https
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")))

;; add melpa to the front
(add-to-list 'package-archives
            '("melpa" . "https://melpa.org/packages/") t)

;; Bootstrap `use-package'
;; http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html
;; use-package autoloads will make sure it get pulled in at the right time
;; read "package autoloads":  http://www.lunaryorn.com/2014/07/02/autoloads-in-emacs-lisp.html
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; magit
(use-package magit
    :ensure t
    :defer t)

;; yasnippet
(use-package yasnippet
    :ensure t)

;; auto close (), {}, [], ""
(electric-pair-mode 1)

;; company-mode
(use-package company
    :ensure t)
(add-hook 'after-init-hook 'global-company-mode)

(use-package flycheck
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
(tool-bar-mode 1)
(scroll-bar-mode 1)
(menu-bar-mode t)
(blink-cursor-mode 0)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;(set-foreground-color "#DAB98F")
;;(set-background-color "#161616")
(set-cursor-color "Lime")
(set-face-attribute 'default nil :height 110)

(setq-default indicate-empty-lines t)
(setq visible-bell t)
(setq default-major-mode 'indented-text-mode)
(setq text-mode-hook 'turn-on-auto-fill)
(setq fill-column 100)

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
(global-set-key [f2] 'save-buffer)
(global-set-key [f4] 'kill-this-buffer)
(global-set-key [f8] 'delete-trailing-whitespace)
(global-set-key [f9] 'compile)
(global-set-key [f10] 'compile)
(global-set-key [(meta g)] 'goto-line)
(global-set-key (kbd "C-q") 'er/expand-region)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-x f") 'find-file)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c r") 'replace-in-buffer)

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
