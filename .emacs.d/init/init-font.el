;; Set default font

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

(add-to-list 'default-frame-alist
             '(font . "monospace 11"))

;; Helpful function to blow up font for presentations.
(defun presentation-sized-font ()
  (interactive)
  (text-scale-adjust 4))

(provide 'init-font)
