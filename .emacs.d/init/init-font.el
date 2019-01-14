;; Set default font
(add-to-list 'default-frame-alist
             '(font . "monospace"))

;; Helpful function to blow up font for presentations.
(defun presentation-sized-font ()
  (interactive)
  (text-scale-adjust 4))

(provide 'init-font)
