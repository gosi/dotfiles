;; Save theme locally if there is no MELPA package.
(add-to-list 'custom-theme-load-path "~/.emacs.d/color-themes/")

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)

;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------

(setq color-theme-is-global t)
(color-theme-initialize)

(set-foreground-color "wheat")
(set-background-color "#161616")
(set-cursor-color "green")

(provide 'init-colors)
