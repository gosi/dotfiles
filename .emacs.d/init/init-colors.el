;; Save theme locally if there is no MELPA package.
;(add-to-list 'custom-theme-load-path "~/.emacs.d/color-themes/")

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)

(if (display-graphic-p)
   (progn (setq default-frame-alist
             (cons (cons 'reverse t) default-frame-alist))
     (make-frame)
     (delete-other-frames) )
  )

(require 'moe-theme)
(load-theme moe-light)
;(set-foreground-color "black")
;(set-background-color "white")
;;(set-cursor-color "green")

(provide 'init-colors)
