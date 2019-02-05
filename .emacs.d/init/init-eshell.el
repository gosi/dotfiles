;; eshell now defines a lousy clear function so we need to define this
;; one as late as possible.

;; Eshell C-l support
 (defun eshell-clear-buffer ()
 "Clear terminal"
 (interactive)
 (let ((inhibit-read-only t))
 (erase-buffer)
 (eshell-send-input)))
 (add-hook 'eshell-mode-hook
 '(lambda()
 (local-set-key (kbd
"C-l") 'eshell-clear-buffer)))

;(add-hook 'eshell-mode-hook
;          (lambda ()
;            ;; clear the buffer in eshell
;            (defun eshell/clear ()
;              "clear the eshell buffer."
;              (interactive)
;              (let ((inhibit-read-only t))
;                (erase-buffer)
;                (recenter-top-bottom 0)))))
;
(defalias 'ff 'find-file)
(defalias 'ffo 'find-file-other-window)


(provide 'init-eshell)
