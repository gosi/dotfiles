;; Set default font

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono 14"))

;; Helpful function to blow up font for presentations.
(defun presentation-sized-font ()
  (interactive)
  (text-scale-adjust 4))

(provide 'init-font)
