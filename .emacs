;; +-----------------------------------------------------------------+
;; | Packages and general stuff                                      |
;; +-----------------------------------------------------------------+
(add-to-list 'load-path "~/.emacs.d/lisp")
(setq debug-on-error t)

;; Setup package.el
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package paredit
   :ensure t)
(dolist (package '(auto-highlight-symbol expand-region company flycheck yasnippet)) ; <--P A C K A G E S
 (unless (package-installed-p package)
   (package-install package))
   (require package))

(global-auto-highlight-symbol-mode)
(setq ahs-idle-interval 1.0) ;; if you want instant highlighting, set it to 0, but I find it annoying
(setq ahs-default-range 'ahs-range-whole-buffer) ;; highlight every occurence in buffer

;; inhibits highlighting in specific places, like in comments
(setq ahs-inhibit-face-list '(font-lock-comment-delimiter-face
                                font-lock-comment-face
                                font-lock-doc-face
                                font-lock-doc-string-face
                                font-lock-string-face))

;; yasnippet
(yas-global-mode 1)

;; auto close (), {}, [], ""
(electric-pair-mode 1)

(global-company-mode 1)
(add-hook 'after-init-hook 'global-flycheck-mode)

;; why isn't this default?
(progn
  (delete-selection-mode 1)
  (transient-mark-mode 1)
  )

(fset 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(desktop-load-default)
(desktop-read)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

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
(tool-bar-mode -1)
(scroll-bar-mode 1)
(menu-bar-mode t)
(blink-cursor-mode 0)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(set-foreground-color "#DAB98F")
(set-background-color "#161616")
(set-cursor-color "Lime")
(set-face-attribute 'default nil :height 110)

(setq show-trailing-whitespace)
(setq-default indicate-empty-lines t)
(setq visible-bell t) ;;removes alarm sound and flashes screen instead
(setq default-major-mode 'indented-text-mode)
(setq text-mode-hook 'turn-on-auto-fill)
(setq fill-column 80)

(require 'whitespace)
(setq whitespace-style '(face tabs lines-tail trailing))
(global-whitespace-mode t)

;; +-----------------------------------------------------------------+
;; | Keyboard shortcuts etc                                          |
;; +-----------------------------------------------------------------+
(global-set-key [f2] 'save-buffer)
(global-set-key [f4] 'kill-this-buffer)
(global-set-key [f8] 'delete-trailing-whitespace)
(global-set-key [f9] 'compile)
(global-set-key [(meta g)] 'goto-line)
(global-set-key (kbd "C-q") 'er/expand-region)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-x f") 'find-file)
(define-key auto-highlight-symbol-mode-map (kbd "C-<") 'ahs-backward)
(define-key auto-highlight-symbol-mode-map (kbd "C->") 'ahs-forward)

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
