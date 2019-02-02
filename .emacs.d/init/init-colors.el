(require-package 'color-theme)
;; Save theme locally if there is no MELPA package.
(add-to-list 'custom-theme-load-path "~/.emacs.d/color-themes/")

(setq color-theme-is-global t)
(color-theme-initialize)

(set-foreground-color "wheat")
(set-background-color "#161616")
(set-cursor-color "green")

;; Highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "black")

;; If we are in the terminal we don't want a theme.
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)

(provide 'init-colors)
