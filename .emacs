;; -*- mode: Emacs-Lisp; -*-

(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; All things UTF-8.
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Styling
(set-face-attribute 'default nil :height 120)
;(set-face-attribute 'font-lock-builtin-face nil :foreground "#5c7a9c")
;(set-face-attribute 'font-lock-comment-face nil :foreground "#a9a9a9")
;(set-face-attribute 'font-lock-constant-face nil :foreground "#8fe1c8")
;(set-face-attribute 'font-lock-function-name-face nil :foreground "FF9F00")
;(set-face-attribute 'font-lock-keyword-face nil :foreground "#5c7a9c")
;(set-face-attribute 'font-lock-string-face nil :foreground "#ffc0cb")
;(set-face-attribute 'font-lock-variable-name-face nil :foreground "#d7af5f")
;(set-face-attribute 'fringe nil :background nil)
;(set-face-foreground 'mode-line "white")
;(set-face-background 'mode-line "limegreen")
;(set-foreground-color "#FFFFFF")
;(set-background-color "#000000")
;(set-cursor-color "#97FF97")

;(load-theme 'distinguished t)
;; Startup windowing
(setq inhibit-startup-screen t)
(setq next-line-add-newlines nil)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
(menu-bar-mode -1)
(scroll-bar-mode 1)
(tool-bar-mode 0)
;(maximize-window)
(display-time)

;; Tramp for remote access
(setq tramp-default-method "ssh")

;; Make use of ido and flex matching
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Not interested in annoying files
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; Mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Delete the selection with a keypress
(delete-selection-mode t)

;; Pairs
;(highlight-parentheses)
;(define-globalized-minor-mode global-highlight-parens highlight-parentheses-mode
;  (lambda ()
;    (highlight-parentheses-mode 1)))
;
;(global-highlight-parens 1)

;; Keep cursor location when scrolling
(setq set-mark-command-repeat-pop t)
(setq scroll-preserve-screen-position t)

;; Stop Emacs from losing undo information by
;; setting very high limits for undo buffers
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

;; Stop prompting me
(fset 'yes-or-no-p 'y-or-n-p)
;; I don't care if the process is running
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
        kill-buffer-query-functions))

;; Key bindings
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c r") 'recompile)
(global-set-key (kbd "C-c b") 'browse-url-at-point)
(global-set-key (kbd "C-o")   'other-window)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(defun indent-buffer ()
      (interactive)
      (save-excursion
        (indent-region (point-min) (point-max) nil)))
(global-set-key [f12] 'indent-buffer)

;; Make C-w behave like in the shell
(defun kill-region-or-backward-word ()
  "If the region is active and non-empty, call `kill-region'.
Otherwise, call `backward-kill-word'."
  (interactive)
  (call-interactively
   (if (use-region-p) 'kill-region 'backward-kill-word)))
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)

(defun px-match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert <key>."
  (interactive "p")
  (cond
   ((char-equal 41 (char-before)) (backward-list 1))
   ((char-equal 125 (char-before)) (backward-list 1))
   ((and
     (char-equal 123 (char-before))
     (char-equal 10 (char-after)))
    (backward-char 1) (forward-list 1))
   ((looking-at "\\s\(") (forward-list 1))
   ((looking-at "\\s\)") (backward-list 1))
   (t (self-insert-command (or arg 1)))))
(global-set-key (kbd "C-q") 'px-match-paren)

;; Quickly jump to saved location
(defun file-point-to-register ()
  "Store cursorposition _fast_ in a register. Use file-jump-to-register
to jump back to the stored position."
  (interactive)
  (message "Saved point to register in %s" (buffer-name))
  (point-to-register 8))

(defun file-jump-to-register ()
  "Switches between current cursorposition and position
that was stored with ska-point-to-register."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp)))
(global-set-key (kbd "C-,") 'file-point-to-register)
(global-set-key (kbd "C-.") 'file-jump-to-register)

;; Random functions
(defun delete-file-and-buffer ()
  "Kills the current buffer and deletes the file it is visiting"
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (delete-file filename)
      (message "Deleted file %s" filename)))
  (kill-buffer))

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun strlen ()
  "Calculate the length of string under cursor."
  (interactive)
  (save-excursion
    (save-restriction
          (widen)
          (let ((start-pos (search-backward "\"")))
            (forward-char 1)
            (message "string length: %d" (- (search-forward "\"") start-pos 2))))))

(defun window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (ujelly_type)))
 '(custom-safe-themes
   (quote
    ("cdb4ffdecc682978da78700a461cdc77456c3a6df1c1803ae2dd55c59fa703e3" "0d456bc74e0ffa4bf5b69b0b54dac5104512c324199e96fc9f3a1db10dfa31f3" "e9903dfe9ff8173e607d62fd2af073631797fd6398f2d7939120d0277d82e9ae" "1b23925543303ed91979cf8751562501a2221dfe812df379b9c69d9dac574623" "ab8e1f25da50a3700bcb37fa468d1f31bff91fcf80b796d3e42a64b03c0cf579" "5a0e9ce828559960cc5dcd35c48aa45a72c3231f1d2f59d8a450f9ae99eeed48" "7f9dc0c7bc8e5b4a1b9904359ee449cac91fd89dde6aca7a45e4ed2e4985664c" "4138944fbed88c047c9973f68908b36b4153646a045648a22083bd622d1e636d" default)))
 '(linum-format " %3i ")
 '(package-selected-packages
   (quote
    (zenburn-theme flycheck jbeans-theme ujelly-theme distinguished-theme evil-commentary evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
